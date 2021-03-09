package special.sigma

import java.math.BigInteger

import org.ergoplatform.ErgoBox
import org.ergoplatform.settings.ErgoAlgos
import org.scalacheck.Gen.containerOfN
import sigmastate._
import org.scalacheck.{Arbitrary, Gen}
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.eval.{Colls, _}
import sigmastate.eval.Extensions._
import org.scalacheck.util.Buildable
import scalan.RType
import scorex.crypto.hash.{Digest32, Blake2b256}
import scorex.crypto.authds.{ADDigest, ADKey, ADValue}
import scorex.util.ModifierId
import sigmastate.Values._
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.ProveDHTuple
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.Values.ErgoTree
import sigmastate.serialization.ErgoTreeSerializer
import sigmastate.serialization.generators.ObjectGenerators
import sigmastate.utils.Helpers
import special.collection.Coll

import scala.reflect.ClassTag

trait SigmaTestingData extends SigmaTestingCommons with ObjectGenerators {
  def Coll[T](items: T*)(implicit cT: RType[T]) = CostingSigmaDslBuilder.Colls.fromItems(items:_*)

  def collOfN[T: RType: Arbitrary](n: Int)(implicit b: Buildable[T, Array[T]]): Gen[Coll[T]] = {
    implicit val g: Gen[T] = Arbitrary.arbitrary[T]
    containerOfN[Array, T](n, g).map(Colls.fromArray(_))
  }

  val bytesGen: Gen[Array[Byte]] = for {
    len <- Gen.choose(0, 100)
    arr <- containerOfN[Array, Byte](len, Arbitrary.arbByte.arbitrary)
  } yield arr

  val bytesCollGen = bytesGen.map(Colls.fromArray(_))
  val intsCollGen = arrayGen[Int].map(Colls.fromArray(_))

  implicit val arbBytes = Arbitrary(bytesCollGen)
  implicit val arbInts = Arbitrary(intsCollGen)

  val keyCollGen = collOfN[Byte](32)

  import org.ergoplatform.dsl.AvlTreeHelpers._

  def createAvlTreeAndProver(entries: (Coll[Byte], Coll[Byte])*) = {
    val kvs = entries.map { case (k,v) => ADKey @@ k.toArray -> ADValue @@ v.toArray}
    val res = createAvlTree(AvlTreeFlags.AllOperationsAllowed, kvs:_*)
    res
  }

  protected def sampleAvlProver = {
    val keys   = arrayOfN(100, keyCollGen).sample.get
    val values = arrayOfN(100, bytesCollGen).sample.get
    val (tree, prover) = createAvlTreeAndProver(keys.zip(values):_*)
    (keys, values, tree, prover)
  }

  protected def sampleAvlTree: AvlTree = {
    val (_, _, _, avlProver) = sampleAvlProver
    val digest = avlProver.digest.toColl
    val tree = SigmaDsl.avlTree(AvlTreeFlags.ReadOnly.serializeToByte, digest, 32, None)
    tree
  }

  val tokenId1: Digest32 = Blake2b256("id1")
  val tokenId2: Digest32 = Blake2b256("id2")

  val header1: Header = CHeader(Blake2b256("Header.id").toColl,
    0,
    Blake2b256("Header.parentId").toColl,
    Blake2b256("ADProofsRoot").toColl,
    sampleAvlTree,
    Blake2b256("transactionsRoot").toColl,
    timestamp = 0,
    nBits = 0,
    height = 0,
    extensionRoot = Blake2b256("transactionsRoot").toColl,
    minerPk = SigmaDsl.groupGenerator,
    powOnetimePk = SigmaDsl.groupGenerator,
    powNonce = Colls.fromArray(Array[Byte](0, 1, 2, 3, 4, 5, 6, 7)),
    powDistance = SigmaDsl.BigInt(BigInt("1405498250268750867257727119510201256371618473728619086008183115260323").bigInteger),
    votes = Colls.fromArray(Array[Byte](0, 1, 2))
  )
  val header2: Header = CHeader(Blake2b256("Header2.id").toColl,
    0,
    header1.id,
    Blake2b256("ADProofsRoot2").toColl,
    sampleAvlTree,
    Blake2b256("transactionsRoot2").toColl,
    timestamp = 2,
    nBits = 0,
    height = 1,
    extensionRoot = Blake2b256("transactionsRoot2").toColl,
    minerPk = SigmaDsl.groupGenerator,
    powOnetimePk = SigmaDsl.groupGenerator,
    powNonce = Colls.fromArray(Array.fill(0.toByte)(8)),
    powDistance = SigmaDsl.BigInt(BigInt("19306206489815517413186395405558417825367537880571815686937307203793939").bigInteger),
    votes =  Colls.fromArray(Array[Byte](0, 1, 0))
  )
  val headers = Colls.fromItems(header2, header1)
  val preHeader: PreHeader = CPreHeader(0,
    header2.id,
    timestamp = 3,
    nBits = 0,
    height = 2,
    minerPk = SigmaDsl.groupGenerator,
    votes = Colls.emptyColl[Byte]
  )

  object TestData {
    val BigIntZero: BigInt = CBigInt(new BigInteger("0", 16))
    val BigIntOne: BigInt = CBigInt(new BigInteger("1", 16))
    val BigIntMinusOne: BigInt = CBigInt(new BigInteger("-1", 16))
    val BigInt10: BigInt = CBigInt(new BigInteger("a", 16))
    val BigInt11: BigInt = CBigInt(new BigInteger("b", 16))

    val BigIntMaxValueStr = "7F" + "ff" * 31
    val BigIntMaxValue_instances = new CloneSet(1000,
      CBigInt(new BigInteger(BigIntMaxValueStr, 16)))

    def createBigIntMaxValue(): BigInt = BigIntMaxValue_instances.getNext

    // TODO HF: this values have bitCount == 255 (see to256BitValueExact)
    val BigIntMinValue = CBigInt(new BigInteger("-7F" + "ff" * 31, 16))
    val BigIntMaxValue = createBigIntMaxValue()
    val BigIntOverlimit = CBigInt(new BigInteger("7F" + "ff" * 33, 16))

    val ge1str = "03358d53f01276211f92d0aefbd278805121d4ff6eb534b777af1ee8abae5b2056"
    val ge2str = "02dba7b94b111f3894e2f9120b577da595ec7d58d488485adf73bf4e153af63575"
    val ge3str = "0290449814f5671172dd696a61b8aa49aaa4c87013f56165e27d49944e98bc414d"

    val ge1_bytes = ErgoAlgos.decodeUnsafe(ge1str)

    class CloneSet[T: ClassTag](val size: Int, generator: => T) {
      val instances = Array.fill(size)(generator)
      var currentInst: Int = 0

      /** Selects next instance (round-robin). */
      def getNext: T = {
        val res = instances(currentInst)
        currentInst = (currentInst + 1) % size
        res
      }
    }

    val ge1_instances = new CloneSet(1000, SigmaDsl.decodePoint(Colls.fromArray(ge1_bytes)))

    /** Selects next ge1 instance (round-robin). */
    def create_ge1(): GroupElement = ge1_instances.getNext

    val ge1 = create_ge1()

    val ge2_bytes = ErgoAlgos.decodeUnsafe(ge2str)
    val ge2_instances = new CloneSet(1000, SigmaDsl.decodePoint(Colls.fromArray(ge2_bytes)))
    def create_ge2(): GroupElement = ge2_instances.getNext
    val ge2 = create_ge2()

    val ge3 = Helpers.decodeGroupElement(ge3str)

    val t1_instances = new CloneSet(1000, CAvlTree(
      AvlTreeData(
        ADDigest @@ ErgoAlgos.decodeUnsafe("000183807f66b301530120ff7fc6bd6601ff01ff7f7d2bedbbffff00187fe89094"),
        AvlTreeFlags(false, true, true),
        1,
        Some(1)
      )
    ))

    def create_t1(): AvlTree = t1_instances.getNext

    val t1: AvlTree = create_t1()
    val t2: AvlTree = CAvlTree(
      AvlTreeData(
        ADDigest @@ ErgoAlgos.decodeUnsafe("ff000d937f80ffd731ed802d24358001ff8080ff71007f00ad37e0a7ae43fff95b"),
        AvlTreeFlags(false, false, false),
        32,
        Some(64)
      )
    )
    val t3: AvlTree = CAvlTree(
      AvlTreeData(
        ADDigest @@ ErgoAlgos.decodeUnsafe("3100d2e101ff01fc047c7f6f00ff80129df69a5090012f01ffca99f5bfff0c8036"),
        AvlTreeFlags(true, false, false),
        128,
        None
      )
    )

    val b1_instances = new CloneSet(1000, CostingBox(
      false,
      new ErgoBox(
        9223372036854775807L,
        new ErgoTree(
          16.toByte,
          Array(
            SigmaPropConstant(
              CSigmaProp(
                ProveDlog(
                  Helpers.decodeECPoint(
                    "0297c44a12f4eb99a85d298fa3ba829b5b42b9f63798c980ece801cc663cc5fc9e"
                  )
                )
              )
            )
          ),
          Right(ConstantPlaceholder(0, SSigmaProp))
        ),
        Coll(
          (Digest32 @@ (ErgoAlgos.decodeUnsafe("6e789ab7b2fffff12280a6cd01557f6fb22b7f80ff7aff8e1f7f15973d7f0001")),
              10000000L),
          (Digest32 @@ (ErgoAlgos.decodeUnsafe("a3ff007f00057600808001ff8f8000019000ffdb806fff7cc0b6015eb37fa600")),
              500L)
        ),
        Map(
          ErgoBox.R5 -> ByteArrayConstant(Helpers.decodeBytes("7fc87f7f01ff")),
          ErgoBox.R4 -> FalseLeaf
        ),
        ModifierId @@ ("218301ae8000018008637f0021fb9e00018055486f0b514121016a00ff718080"),
        22588.toShort,
        677407
      )
    ))

    def create_b1(): Box = b1_instances.getNext

    val b1: Box = create_b1()

    val b2: Box = CostingBox(
      false,
      new ErgoBox(
        12345L,
        new ErgoTree(
          0.toByte,
          Vector(),
          Right(
            BoolToSigmaProp(
              AND(
                ConcreteCollection(
                  Array(
                    FalseLeaf,
                    XorOf(
                      ConcreteCollection(Array(EQ(IntConstant(1), IntConstant(1)), FalseLeaf), SBoolean)
                    )
                  ),
                  SBoolean
                )
              )
            )
          )
        ),
        Coll(),
        Map(
          ErgoBox.R5 -> ByteArrayConstant(
            Helpers.decodeBytes(
              "297000800b80f1d56c809a8c6affbed864b87f007f6f007f00ac00018c01c4fdff011088807f0100657f00f9ab0101ff6d65"
            )
          ),
          ErgoBox.R4 -> TrueLeaf,
          ErgoBox.R7 -> LongConstant(9223372036854775807L),
          ErgoBox.R6 -> LongConstant(2115927197107005906L)
        ),
        ModifierId @@ ("003bd5c630803cfff6c1ff7f7fb980ff136afc011f8080b8b04ad4dbda2d7f4e"),
        1.toShort,
        1000000
      )
    )

    val preH1_instances = new CloneSet(1000, CPreHeader(
      0.toByte,
      Helpers.decodeBytes("7fff7fdd6f62018bae0001006d9ca888ff7f56ff8006573700a167f17f2c9f40"),
      6306290372572472443L,
      -3683306095029417063L,
      1,
      Helpers.decodeGroupElement("026930cb9972e01534918a6f6d6b8e35bc398f57140d13eb3623ea31fbd069939b"),
      Helpers.decodeBytes("ff8087")
    ))

    def create_preH1(): PreHeader = preH1_instances.getNext

    val preH1: PreHeader = create_preH1()

    val preH2: PreHeader = create_preH1().asInstanceOf[CPreHeader].copy(height = 2)

    def createAvlTreeData() = AvlTreeData(
      ADDigest @@ (
          ErgoAlgos.decodeUnsafe("010180017f7f7b7f720c00007f7f7f0f01e857a626f37f1483d06af8077a008080")
          ),
      AvlTreeFlags(false, true, false),
      728138553,
      Some(2147483647)
    )

    val h1_instances = new CloneSet(1000, CHeader(
      Helpers.decodeBytes("957f008001808080ffe4ffffc8f3802401df40006aa05e017fa8d3f6004c804a"),
      0.toByte,
      Helpers.decodeBytes("0180dd805b0000ff5400b997fd7f0b9b00de00fb03c47e37806a8186b94f07ff"),
      Helpers.decodeBytes("01f07f60d100ffb970c3007f60ff7f24d4070bb8fffa7fca7f34c10001ffe39d"),
      CAvlTree(createAvlTreeData()),
      Helpers.decodeBytes("804101ff01000080a3ffbd006ac080098df132a7017f00649311ec0e00000100"),
      1L,
      -1L,
      1,
      Helpers.decodeBytes("e57f80885601b8ff348e01808000bcfc767f2dd37f0d01015030ec018080bc62"),
      Helpers.decodeGroupElement("039bdbfa0b49cc6bef58297a85feff45f7bbeb500a9d2283004c74fcedd4bd2904"),
      Helpers.decodeGroupElement("0361299207fa392231e23666f6945ae3e867b978e021d8d702872bde454e9abe9c"),
      Helpers.decodeBytes("7f4f09012a807f01"),
      CBigInt(new BigInteger("-e24990c47e15ed4d0178c44f1790cc72155d516c43c3e8684e75db3800a288", 16)),
      Helpers.decodeBytes("7f0180")
    ))

    def create_h1(): Header = h1_instances.getNext

    val h1: Header = create_h1()

    val h2: Header = create_h1().asInstanceOf[CHeader].copy(height = 2)

    val dlog_instances = new CloneSet(1000, ProveDlog(
      SigmaDsl.toECPoint(create_ge1()).asInstanceOf[EcPointType]
    ))

    def create_dlog(): ProveDlog = dlog_instances.getNext

    val dht_instances = new CloneSet(1000, ProveDHTuple(
      create_dlog().value,
      SigmaDsl.toECPoint(create_ge2()).asInstanceOf[EcPointType],
      create_dlog().value,
      SigmaDsl.toECPoint(create_ge2()).asInstanceOf[EcPointType]
    ))

    def create_dht(): ProveDHTuple = dht_instances.getNext

    def create_and() = CAND(Array(create_dlog(), create_dht()))

    def create_or() = COR(Array(create_dlog(), create_dht()))
  }

  /** A list of ErgoTree hexes which are the most often used in Ergo mainnet. */
  val predefScriptHexes = Seq(
    "0008cd03a40b2249d6a9cc7eedf21188842acef44f3df110a58a687ba3e28cbc2e97ead2",
    "1005040004000e36100204a00b08cd0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798ea02d192a39a8cc7a701730073011001020402d19683030193a38cc7b2a57300000193c2b2a57301007473027303830108cdeeac93b1a57304",
    "101004020e36100204a00b08cd0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798ea02d192a39a8cc7a7017300730110010204020404040004c0fd4f05808c82f5f6030580b8c9e5ae040580f882ad16040204c0944004c0f407040004000580f882ad16d19683030191a38cc7a7019683020193c2b2a57300007473017302830108cdeeac93a38cc7b2a573030001978302019683040193b1a5730493c2a7c2b2a573050093958fa3730673079973089c73097e9a730a9d99a3730b730c0599c1a7c1b2a5730d00938cc7b2a5730e0001a390c1a7730f",
    "10160e201a6a8c16e4b1cc9d73d03183565cfb8e79dd84198cb66beeed7d3463e0da2b9805000500040008cd03b038b0783c899be6b5b98bcf55df573c87cb2e01c16604c174e5a7e6105e848e04040406040204000400040604080402040004000402040405000500050205040100d808d601b1a4d602c2a7d603c6a70405d6047300d605860272047301d6067302d607d9010763d806d609c27207d60a9372097202d60bd805d60bc17207d60cc1a7d60de47203d60e99720c720dd60f92720b720e720fd60ced720a720bd60dd802d60dc672070405d60e93720d7203720ed60eed720c720d720ed608d9010863d806d60adb63087208d60bb2720a7303017205d60c8c720b01d60d93720c7204d60ed801d60e8c720b02720ed60f95720d720e7206720feb02730495ed937201730593b1a57306d802d6097207d60a7208d1edda720901b2a57307008fda720a01b2a5730800da720a01b2a473090095ed937201730a93b1a5730bd803d609da720801b2a4730c00d60ada720801b2a4730d00d60b999a720a72099ada720801b2a5730e00da720801b2a5730f00d1edda720701b2a5731000eded917209731191720a7312ec93720b731393720b7314d17315",
    "10060e2002d1541415c323527f19ef5b103eb33c220ea8b66fcb711806b0037d115d63f204000402040004040e201a6a8c16e4b1cc9d73d03183565cfb8e79dd84198cb66beeed7d3463e0da2b98d803d601e4c6a70507d602d901026393cbc27202e4c6a7070ed6037300ea02eb02cd7201cedb6a01dde4c6a70407e4c6a706077201d1ececedda720201b2a573010093cbc2b2a47302007203edda720201b2a473030093cbc2b2a47304007203afa5d9010463afdb63087204d901064d0e948c7206017305",
    "100d0e201a6a8c16e4b1cc9d73d03183565cfb8e79dd84198cb66beeed7d3463e0da2b9804000e20c9ffb7bf74cd7a0fc2b76baf54b4c6192b0a1689e6b0ea6b5d988447c353a3ee0400040004020504040205000402040204000100d806d601e4c6a70407d6027300d603b2a5730100d604c672030407d6057302d606db6a01ddeb02ea02cd7201d1afa5d9010763afdb63087207d901094d0e948c720901720295e67204d808d607db63087203d608b27207730300d609db6308a7d60ab27209730400d60bd9010b63eded93cbc2720b720593e4c6720b070ecbc2a793c1720bc1a7d60cb2a5730500d60de4c672030507d60ee47204ea02d1edededededededeced938c7208018c720a01919c8c72080273068c720a0293cbc2b2a47307007205edededda720b017203da720b01720c937207db6308720cd801d60f86027202730893b27209730901720fb27207730a01720f93e4c672030607720193e4c6720c0607720193e4c6720c0407720d93e4c6720c0507e4720493c5a7c5b2a4730b0094e47204720deb02ce72067201720e720dce72067201720d720ed1730c",
    "100c040004000e201a6a8c16e4b1cc9d73d03183565cfb8e79dd84198cb66beeed7d3463e0da2b98040005020400040005020402040204000400d805d601e4c6a70507d602d9010263ed93cbc27202e4c6a7070e93c17202c1a7d603b2a5730000d604b2a4730100d6057302ea02eb02cd7201cedb6a01dde4c6a70407e4c6a706077201d1ececededda720201720393c57204c5a7eddad9010663d801d608b2db63087206730300ed938c7208017205928c7208027304017203938cb2db6308720373050002998cb2db63087204730600027307ededda720201720493c5b2a4730800c5a7938cb2db6308b2a4730900730a00017205afa5d901066393b1db63087206730b",
    "100c0e201008c10aea11a275eaac3bffdd08427ec6e80b7512476a89396cd25d415a2de10e206c143ec48214ce2d204c959839c8ddfd0ca030007dba4a95894a0815fe4d41380404040604000400040604080400040205c08db70108cd03b038b0783c899be6b5b98bcf55df573c87cb2e01c16604c174e5a7e6105e848ed803d601b1a4d6027300d6037301eb02d1edecededed937201730293b1a57303dad901046393cbc27204720201b2a573040093cbc2b2a47305007203ededed937201730693b1a57307dad901046393cbc27204720201b2a473080093cbc2b2a47309007203aea5d9010463ed93c27204c2a792c1720499c1a7730a730b",
    "1000d801d601e4c6a70507eb02cd7201cedb6a01dde4c6a70407e4c6a706077201",
    "100204a00b08cd02b3a06d6eaa8671431ba1db4dd427a77f75a5c2acbd71bfb725d38adc2b55f669ea02d192a39a8cc7a70173007301",
    "100204a00b08cd02ebaaeb381c9d855af1807781fa20ef6c0c34833275ce7913a9e4469f7bcb3becea02d192a39a8cc7a70173007301"
  )

  /** Parses ErgoTree instance from hex string. */
  def parseTree(hex: String): ErgoTree = {
    val bytes = ErgoAlgos.decodeUnsafe(hex)
    val tree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bytes)
    tree
  }
}

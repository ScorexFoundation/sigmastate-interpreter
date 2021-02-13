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
    val key = keyCollGen.sample.get
    val value = bytesCollGen.sample.get
    val (tree, prover) = createAvlTreeAndProver(key -> value)
    (key, value, tree, prover)
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
    val ge2 = Helpers.decodeGroupElement(ge2str)
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

    def create_b1(): CostingBox = {
      CostingBox(
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
      )
    }

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

    def create_preH1(): CPreHeader = {
      CPreHeader(
        0.toByte,
        Helpers.decodeBytes("7fff7fdd6f62018bae0001006d9ca888ff7f56ff8006573700a167f17f2c9f40"),
        6306290372572472443L,
        -3683306095029417063L,
        1,
        Helpers.decodeGroupElement("026930cb9972e01534918a6f6d6b8e35bc398f57140d13eb3623ea31fbd069939b"),
        Helpers.decodeBytes("ff8087")
      )
    }

    val preH1: PreHeader = create_preH1()

    val preH2: PreHeader = create_preH1().copy(height = 2)

    def createAvlTreeData() = AvlTreeData(
      ADDigest @@ (
          ErgoAlgos.decodeUnsafe("010180017f7f7b7f720c00007f7f7f0f01e857a626f37f1483d06af8077a008080")
          ),
      AvlTreeFlags(false, true, false),
      728138553,
      Some(2147483647)
    )

    def create_h1() = {
      CHeader(
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
      )
    }

    val h1: Header = create_h1()

    val h2: Header = create_h1().copy(height = 2)
  }
}

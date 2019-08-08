package sigmastate

import org.ergoplatform.ErgoLikeContext
import org.scalatest.prop.TableFor2
import scorex.util.encode.Base16
import sigmastate.Values.{ByteArrayConstant, CollectionConstant}
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, SigmaTestingCommons}

class CalcSha256Specification extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext

  def stringToByteConstant(in: String): CollectionConstant[SByte.type] = ByteArrayConstant(in.getBytes("UTF-8"))
  def decodeString(in: String): CollectionConstant[SByte.type] = ByteArrayConstant(Base16.decode(in).get)

  /**
    * https://www.di-mgt.com.au/sha_testvectors.html
    *
    * Example table for sha256 hashing.
    * Result is Base16 representation of byte array.
    */
  val objects: TableFor2[String, String] = Table(
    ("string", "hash"),
    ("abc", "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"),
    ("", "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"),
    ("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq", "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"),
    ("abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu", "cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1"),
    (Array.fill[String](1000000)("a").mkString, "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0"),

  )

  property("CalcSha256: Should pass standard tests.") {
    val int = new ContextEnrichingTestProvingInterpreter()
    val ctx = ErgoLikeContextTesting.dummy(fakeSelf)
    forAll(objects) { (in, result) =>
      val expectedResult = decodeString(result)
      val calcSha256 = EQ(CalcSha256(stringToByteConstant(in)), expectedResult)
      val res = int.reduceToCrypto(ctx, calcSha256).get._1
      res shouldBe TrivialProp.TrueProp
    }
  }

}
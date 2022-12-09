package sigmastate

import org.scalatest.prop.TableFor2
import scorex.util.encode.Base16
import sigmastate.Values.{ByteArrayConstant, CollectionConstant}
import sigmastate.crypto.CryptoFacade
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, TestingCommons}

class HashHmacSHA512_Specification extends TestingCommons
  with CrossVersionProps {

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
    ("abc", "2c15e87cde0f876fd8f060993748330cbe5f37c8bb3355e8ef44cea57890ec1d9b3274ef2b67bbe046cf8a012fba69796ec7803b1cc227521b9f5191e80a7da2"),
    ("", "300b155f751964276c0536230bd9b16fe7a86533c3cbaa7575e8d0431dbedf23f9945bb8b052bd0b0802c10c7c852e7765b69b61ce7233d9fe5a35ab108ca3b6"),
    ("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq", "888ead7cb2ff330420333cac103f1062a6a443170108f6f74e2cdf39468015ae792c4a822664ce5d865424d2569d67bec03abd2df2a924977d635d06a0b550a3"),
    ("abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu", "57edbc19570001de233edcb104237ea81439e59c5b0000d4db7bd991d228453827428d3cf30ecb4cdb17f00de3444579fa771f8933d2a7d9430b56dd989e55d9"),
    ("a" * 1000, "2b53959bef11414f0592f8d6e834857734c7b9fcfbbfcafd84db989a85f7dfa76990a832fd24d8a3f804801898e59ac11a4dcd3e23a9121adc40a5498dcf4bc5")
  )

  property("HashHmacSHA512: Should pass standard tests.") {
    forAll(objects) { (in, expectedRes) =>
      val inBytes = in.getBytes(CryptoFacade.Encoding)
      val res = CryptoFacade.hashHmacSHA512(CryptoFacade.BitcoinSeed, inBytes)
      Base16.encode(res) shouldBe expectedRes
    }
  }

}
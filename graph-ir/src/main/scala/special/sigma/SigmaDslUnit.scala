package special.sigma {
  import scalan._

  trait SigmaDsl extends Base { self: SigmaLibrary =>
    trait BigInt extends Def[BigInt] {
      def add(that: Ref[BigInt]): Ref[BigInt];
      def subtract(that: Ref[BigInt]): Ref[BigInt];
      def multiply(that: Ref[BigInt]): Ref[BigInt];
      def divide(that: Ref[BigInt]): Ref[BigInt];
      def mod(m: Ref[BigInt]): Ref[BigInt];
      def min(that: Ref[BigInt]): Ref[BigInt];
      def max(that: Ref[BigInt]): Ref[BigInt];
    };
    trait GroupElement extends Def[GroupElement] {
      def isInfinity: Ref[Boolean];
      def exp(k: Ref[BigInt]): Ref[GroupElement];
      def multiply(that: Ref[GroupElement]): Ref[GroupElement];
      def negate: Ref[GroupElement];
      def getEncoded: Ref[Coll[Byte]]
    };
    trait SigmaProp extends Def[SigmaProp] {
      def isValid: Ref[Boolean];
      def propBytes: Ref[Coll[Byte]];
      def &&(other: Ref[SigmaProp]): Ref[SigmaProp];
      def ||(other: Ref[SigmaProp]): Ref[SigmaProp];
    };
    trait Box extends Def[Box] {
      def id: Ref[Coll[Byte]];
      def value: Ref[Long];
      def propositionBytes: Ref[Coll[Byte]];
      def bytes: Ref[Coll[Byte]];
      def bytesWithoutRef: Ref[Coll[Byte]];
      def getReg[T](i: Ref[Int])(implicit cT: Elem[T]): Ref[WOption[T]];
      def R0[T](implicit cT: Elem[T]): Ref[WOption[T]] = this.getReg[T](toRep(0.asInstanceOf[Int]));
      def R1[T](implicit cT: Elem[T]): Ref[WOption[T]] = this.getReg[T](toRep(1.asInstanceOf[Int]));
      def R2[T](implicit cT: Elem[T]): Ref[WOption[T]] = this.getReg[T](toRep(2.asInstanceOf[Int]));
      def R3[T](implicit cT: Elem[T]): Ref[WOption[T]] = this.getReg[T](toRep(3.asInstanceOf[Int]));
      def R4[T](implicit cT: Elem[T]): Ref[WOption[T]] = this.getReg[T](toRep(4.asInstanceOf[Int]));
      def R5[T](implicit cT: Elem[T]): Ref[WOption[T]] = this.getReg[T](toRep(5.asInstanceOf[Int]));
      def R6[T](implicit cT: Elem[T]): Ref[WOption[T]] = this.getReg[T](toRep(6.asInstanceOf[Int]));
      def R7[T](implicit cT: Elem[T]): Ref[WOption[T]] = this.getReg[T](toRep(7.asInstanceOf[Int]));
      def R8[T](implicit cT: Elem[T]): Ref[WOption[T]] = this.getReg[T](toRep(8.asInstanceOf[Int]));
      def R9[T](implicit cT: Elem[T]): Ref[WOption[T]] = this.getReg[T](toRep(9.asInstanceOf[Int]));
      def tokens: Ref[Coll[scala.Tuple2[Coll[Byte], Long]]];
      def creationInfo: Ref[scala.Tuple2[Int, Coll[Byte]]];
      def executeFromRegister[T](regId: Ref[Byte])(implicit cT: Elem[T]): Ref[T]
    };
    trait AvlTree extends Def[AvlTree] {
      def digest: Ref[Coll[Byte]];
      def enabledOperations: Ref[Byte];
      def keyLength: Ref[Int];
      def valueLengthOpt: Ref[WOption[Int]];
      def isInsertAllowed: Ref[Boolean];
      def isUpdateAllowed: Ref[Boolean];
      def isRemoveAllowed: Ref[Boolean];
      def updateDigest(newDigest: Ref[Coll[Byte]]): Ref[AvlTree];
      def updateOperations(newOperations: Ref[Byte]): Ref[AvlTree];
      def contains(key: Ref[Coll[Byte]], proof: Ref[Coll[Byte]]): Ref[Boolean];
      def get(key: Ref[Coll[Byte]], proof: Ref[Coll[Byte]]): Ref[WOption[Coll[Byte]]];
      def getMany(keys: Ref[Coll[Coll[Byte]]], proof: Ref[Coll[Byte]]): Ref[Coll[WOption[Coll[Byte]]]];
      def insert(operations: Ref[Coll[scala.Tuple2[Coll[Byte], Coll[Byte]]]], proof: Ref[Coll[Byte]]): Ref[WOption[AvlTree]];
      def update(operations: Ref[Coll[scala.Tuple2[Coll[Byte], Coll[Byte]]]], proof: Ref[Coll[Byte]]): Ref[WOption[AvlTree]];
      def remove(operations: Ref[Coll[Coll[Byte]]], proof: Ref[Coll[Byte]]): Ref[WOption[AvlTree]]
    };
    trait PreHeader extends Def[PreHeader] {
      def version: Ref[Byte];
      def parentId: Ref[Coll[Byte]];
      def timestamp: Ref[Long];
      def nBits: Ref[Long];
      def height: Ref[Int];
      def minerPk: Ref[GroupElement];
      def votes: Ref[Coll[Byte]]
    };
    trait Header extends Def[Header] {
      def id: Ref[Coll[Byte]];
      def version: Ref[Byte];
      def parentId: Ref[Coll[Byte]];
      def ADProofsRoot: Ref[Coll[Byte]];
      def stateRoot: Ref[AvlTree];
      def transactionsRoot: Ref[Coll[Byte]];
      def timestamp: Ref[Long];
      def nBits: Ref[Long];
      def height: Ref[Int];
      def extensionRoot: Ref[Coll[Byte]];
      def minerPk: Ref[GroupElement];
      def powOnetimePk: Ref[GroupElement];
      def powNonce: Ref[Coll[Byte]];
      def powDistance: Ref[BigInt];
      def votes: Ref[Coll[Byte]]
    };
    trait Context extends Def[Context] {
      def builder: Ref[SigmaDslBuilder];
      def OUTPUTS: Ref[Coll[Box]];
      def INPUTS: Ref[Coll[Box]];
      def dataInputs: Ref[Coll[Box]];
      def HEIGHT: Ref[Int];
      def SELF: Ref[Box];
      def selfBoxIndex: Ref[Int];
      def LastBlockUtxoRootHash: Ref[AvlTree];
      def headers: Ref[Coll[Header]];
      def preHeader: Ref[PreHeader];
      def minerPubKey: Ref[Coll[Byte]];
      def getVar[T](id: Ref[Byte])(implicit cT: Elem[T]): Ref[WOption[T]];
    };
    trait SigmaDslBuilder extends Def[SigmaDslBuilder] {
      def Colls: Ref[CollBuilder];
      def verifyZK(cond: Ref[Thunk[SigmaProp]]): Ref[Boolean];
      def atLeast(bound: Ref[Int], props: Ref[Coll[SigmaProp]]): Ref[SigmaProp];
      def allOf(conditions: Ref[Coll[Boolean]]): Ref[Boolean];
      def allZK(conditions: Ref[Coll[SigmaProp]]): Ref[SigmaProp];
      def anyOf(conditions: Ref[Coll[Boolean]]): Ref[Boolean];
      def anyZK(conditions: Ref[Coll[SigmaProp]]): Ref[SigmaProp];
      def xorOf(conditions: Ref[Coll[Boolean]]): Ref[Boolean];
      def sigmaProp(b: Ref[Boolean]): Ref[SigmaProp];
      def blake2b256(bytes: Ref[Coll[Byte]]): Ref[Coll[Byte]];
      def sha256(bytes: Ref[Coll[Byte]]): Ref[Coll[Byte]];
      def byteArrayToBigInt(bytes: Ref[Coll[Byte]]): Ref[BigInt];
      def longToByteArray(l: Ref[Long]): Ref[Coll[Byte]];
      def byteArrayToLong(bytes: Ref[Coll[Byte]]): Ref[Long];
      def proveDlog(g: Ref[GroupElement]): Ref[SigmaProp];
      def proveDHTuple(g: Ref[GroupElement], h: Ref[GroupElement], u: Ref[GroupElement], v: Ref[GroupElement]): Ref[SigmaProp];
      def groupGenerator: Ref[GroupElement];
      def substConstants[T](scriptBytes: Ref[Coll[Byte]], positions: Ref[Coll[Int]], newValues: Ref[Coll[T]]): Ref[Coll[Byte]];
      def decodePoint(encoded: Ref[Coll[Byte]]): Ref[GroupElement];
      def avlTree(operationFlags: Ref[Byte], digest: Ref[Coll[Byte]], keyLength: Ref[Int], valueLengthOpt: Ref[WOption[Int]]): Ref[AvlTree];
      def xor(l: Ref[Coll[Byte]], r: Ref[Coll[Byte]]): Ref[Coll[Byte]]
    };
    trait CostModelCompanion;
    trait BigIntCompanion;
    trait GroupElementCompanion;
    trait SigmaPropCompanion;
    trait BoxCompanion;
    trait AvlTreeCompanion;
    trait PreHeaderCompanion;
    trait HeaderCompanion;
    trait ContextCompanion;
    trait SigmaContractCompanion;
    trait SigmaDslBuilderCompanion
  }
}
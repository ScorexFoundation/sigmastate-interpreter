package special.sigma {
  import scalan.OverloadHack.Overloaded1   // manual fix
  import scalan._

  trait SigmaDsl extends Base { self: SigmaLibrary =>
    import AnyValue._;
    import AvlTree._;
    import BigInt._;
    import Box._;
    import Coll._;
    import CollBuilder._;
    import Context._;
    import CostModel._;
    import CostedBuilder._;
    import GroupElement._;
    import Header._;
    import MonoidBuilder._;
    import PreHeader._;
    import SigmaContract._;
    import SigmaDslBuilder._;
    import SigmaProp._;
    import WOption._;
    import WRType._;
    @Liftable trait CostModel extends Def[CostModel] {
      def AccessBox: Ref[Int];
      def AccessAvlTree: Ref[Int];
      def GetVar: Ref[Int];
      def DeserializeVar: Ref[Int];
      def GetRegister: Ref[Int];
      def DeserializeRegister: Ref[Int];
      def SelectField: Ref[Int];
      def CollectionConst: Ref[Int];
      def AccessKiloByteOfData: Ref[Int];
      def PubKeySize: Ref[Long]
    };
    @Liftable @WithMethodCallRecognizers trait BigInt extends Def[BigInt] {
      def toByte: Ref[Byte];
      def toShort: Ref[Short];
      def toInt: Ref[Int];
      def toLong: Ref[Long];
      def toBytes: Ref[Coll[Byte]];
      def toBits: Ref[Coll[Boolean]];
      def toAbs: Ref[BigInt];
      def compareTo(that: Ref[BigInt]): Ref[Int];
      def modQ: Ref[BigInt];
      def plusModQ(other: Ref[BigInt]): Ref[BigInt];
      def minusModQ(other: Ref[BigInt]): Ref[BigInt];
      def multModQ(other: Ref[BigInt]): Ref[BigInt];
      def inverseModQ: Ref[BigInt];
      def signum: Ref[Int];
      def add(that: Ref[BigInt]): Ref[BigInt];
      def subtract(that: Ref[BigInt]): Ref[BigInt];
      def multiply(that: Ref[BigInt]): Ref[BigInt];
      def divide(that: Ref[BigInt]): Ref[BigInt];
      def mod(m: Ref[BigInt]): Ref[BigInt];
      def remainder(that: Ref[BigInt]): Ref[BigInt];
      def min(that: Ref[BigInt]): Ref[BigInt];
      def max(that: Ref[BigInt]): Ref[BigInt];
      def negate: Ref[BigInt]
    };
    @Liftable @WithMethodCallRecognizers trait GroupElement extends Def[GroupElement] {
      def isInfinity: Ref[Boolean];
      def exp(k: Ref[BigInt]): Ref[GroupElement];
      def multiply(that: Ref[GroupElement]): Ref[GroupElement];
      def negate: Ref[GroupElement];
      def getEncoded: Ref[Coll[Byte]]
    };
    @Liftable @WithMethodCallRecognizers trait SigmaProp extends Def[SigmaProp] {
      def isValid: Ref[Boolean];
      def propBytes: Ref[Coll[Byte]];
      @OverloadId(value = "and_sigma") def &&(other: Ref[SigmaProp]): Ref[SigmaProp];
      // manual fix
      @OverloadId(value = "and_bool") def &&(other: Ref[Boolean])(implicit o: Overloaded1): Ref[SigmaProp];
      @OverloadId(value = "or_sigma") def ||(other: Ref[SigmaProp]): Ref[SigmaProp];
      // manual fix
      @OverloadId(value = "or_bool") def ||(other: Ref[Boolean])(implicit o: Overloaded1): Ref[SigmaProp];
    };
    @Liftable @WithMethodCallRecognizers trait AnyValue extends Def[AnyValue] {
      def value: Ref[Any];
      def tVal: Ref[WRType[Any]]
    };
    @Liftable @WithMethodCallRecognizers trait Box extends Def[Box] {
      def id: Ref[Coll[Byte]];
      def value: Ref[Long];
      def propositionBytes: Ref[Coll[Byte]];
      def bytes: Ref[Coll[Byte]];
      def bytesWithoutRef: Ref[Coll[Byte]];
      def registers: Ref[Coll[AnyValue]];
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
    @Liftable trait AvlTree extends Def[AvlTree] {
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
    @Liftable trait PreHeader extends Def[PreHeader] {
      def version: Ref[Byte];
      def parentId: Ref[Coll[Byte]];
      def timestamp: Ref[Long];
      def nBits: Ref[Long];
      def height: Ref[Int];
      def minerPk: Ref[GroupElement];
      def votes: Ref[Coll[Byte]]
    };
    @Liftable trait Header extends Def[Header] {
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
    @Liftable @WithMethodCallRecognizers trait Context extends Def[Context] {
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
      def vars: Ref[Coll[AnyValue]]
    };
    @Liftable trait SigmaContract extends Def[SigmaContract] {
      def builder: Ref[SigmaDslBuilder];
      @NeverInline @Reified(value = "T") def Collection[T](items: Ref[T]*)(implicit cT: Elem[T]): Ref[Coll[T]] = delayInvoke;
      def verifyZK(cond: Ref[Thunk[SigmaProp]]): Ref[Boolean] = this.builder.verifyZK(cond);
      def atLeast(bound: Ref[Int], props: Ref[Coll[SigmaProp]]): Ref[SigmaProp] = this.builder.atLeast(bound, props);
      def allOf(conditions: Ref[Coll[Boolean]]): Ref[Boolean] = this.builder.allOf(conditions);
      def allZK(conditions: Ref[Coll[SigmaProp]]): Ref[SigmaProp] = this.builder.allZK(conditions);
      def anyOf(conditions: Ref[Coll[Boolean]]): Ref[Boolean] = this.builder.anyOf(conditions);
      def anyZK(conditions: Ref[Coll[SigmaProp]]): Ref[SigmaProp] = this.builder.anyZK(conditions);
      def xorOf(conditions: Ref[Coll[Boolean]]): Ref[Boolean] = this.builder.xorOf(conditions);
      def PubKey(base64String: Ref[String]): Ref[SigmaProp] = this.builder.PubKey(base64String);
      def sigmaProp(b: Ref[Boolean]): Ref[SigmaProp] = this.builder.sigmaProp(b);
      def blake2b256(bytes: Ref[Coll[Byte]]): Ref[Coll[Byte]] = this.builder.blake2b256(bytes);
      def sha256(bytes: Ref[Coll[Byte]]): Ref[Coll[Byte]] = this.builder.sha256(bytes);
      def byteArrayToBigInt(bytes: Ref[Coll[Byte]]): Ref[BigInt] = this.builder.byteArrayToBigInt(bytes);
      def longToByteArray(l: Ref[Long]): Ref[Coll[Byte]] = this.builder.longToByteArray(l);
      def byteArrayToLong(bytes: Ref[Coll[Byte]]): Ref[Long] = this.builder.byteArrayToLong(bytes);
      def proveDlog(g: Ref[GroupElement]): Ref[SigmaProp] = this.builder.proveDlog(g);
      def proveDHTuple(g: Ref[GroupElement], h: Ref[GroupElement], u: Ref[GroupElement], v: Ref[GroupElement]): Ref[SigmaProp] = this.builder.proveDHTuple(g, h, u, v);
      def groupGenerator: Ref[GroupElement] = this.builder.groupGenerator;
      def decodePoint(encoded: Ref[Coll[Byte]]): Ref[GroupElement] = this.builder.decodePoint(encoded);
      @Reified(value = "T") def substConstants[T](scriptBytes: Ref[Coll[Byte]], positions: Ref[Coll[Int]], newValues: Ref[Coll[T]])(implicit cT: Elem[T]): Ref[Coll[Byte]] = this.builder.substConstants[T](scriptBytes, positions, newValues)
    };
    @Liftable @WithMethodCallRecognizers trait SigmaDslBuilder extends Def[SigmaDslBuilder] {
      def Colls: Ref[CollBuilder];
      def Monoids: Ref[MonoidBuilder];
      def Costing: Ref[CostedBuilder];
      def CostModel: Ref[CostModel];
      def verifyZK(cond: Ref[Thunk[SigmaProp]]): Ref[Boolean];
      def atLeast(bound: Ref[Int], props: Ref[Coll[SigmaProp]]): Ref[SigmaProp];
      def allOf(conditions: Ref[Coll[Boolean]]): Ref[Boolean];
      def allZK(conditions: Ref[Coll[SigmaProp]]): Ref[SigmaProp];
      def anyOf(conditions: Ref[Coll[Boolean]]): Ref[Boolean];
      def anyZK(conditions: Ref[Coll[SigmaProp]]): Ref[SigmaProp];
      def xorOf(conditions: Ref[Coll[Boolean]]): Ref[Boolean];
      def PubKey(base64String: Ref[String]): Ref[SigmaProp];
      def sigmaProp(b: Ref[Boolean]): Ref[SigmaProp];
      def blake2b256(bytes: Ref[Coll[Byte]]): Ref[Coll[Byte]];
      def sha256(bytes: Ref[Coll[Byte]]): Ref[Coll[Byte]];
      def byteArrayToBigInt(bytes: Ref[Coll[Byte]]): Ref[BigInt];
      def longToByteArray(l: Ref[Long]): Ref[Coll[Byte]];
      def byteArrayToLong(bytes: Ref[Coll[Byte]]): Ref[Long];
      def proveDlog(g: Ref[GroupElement]): Ref[SigmaProp];
      def proveDHTuple(g: Ref[GroupElement], h: Ref[GroupElement], u: Ref[GroupElement], v: Ref[GroupElement]): Ref[SigmaProp];
      def groupGenerator: Ref[GroupElement];
      @Reified(value = "T") def substConstants[T](scriptBytes: Ref[Coll[Byte]], positions: Ref[Coll[Int]], newValues: Ref[Coll[T]])(implicit cT: Elem[T]): Ref[Coll[Byte]];
      def decodePoint(encoded: Ref[Coll[Byte]]): Ref[GroupElement];
      def avlTree(operationFlags: Ref[Byte], digest: Ref[Coll[Byte]], keyLength: Ref[Int], valueLengthOpt: Ref[WOption[Int]]): Ref[AvlTree];
      def xor(l: Ref[Coll[Byte]], r: Ref[Coll[Byte]]): Ref[Coll[Byte]]
    };
    trait CostModelCompanion;
    trait BigIntCompanion;
    trait GroupElementCompanion;
    trait SigmaPropCompanion;
    trait AnyValueCompanion;
    trait BoxCompanion;
    trait AvlTreeCompanion;
    trait PreHeaderCompanion;
    trait HeaderCompanion;
    trait ContextCompanion;
    trait SigmaContractCompanion;
    trait SigmaDslBuilderCompanion
  }
}
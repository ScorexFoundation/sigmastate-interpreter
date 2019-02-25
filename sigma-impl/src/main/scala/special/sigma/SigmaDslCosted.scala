package special.sigma

import special.SpecialPredef
import special.collection.{Coll, CCostedPrim, _}

import scala.reflect.ClassTag
import scalan.RType
import scalan.{NeverInline, Reified}

class CCostedContext(val ctx: Context) extends CostedContext {
  def dsl: SigmaDslBuilder = new TestSigmaDslBuilder
  def dataInputs: CostedColl[Box] = dsl.costBoxes(ctx.dataInputs)
  def OUTPUTS: CostedColl[Box] = dsl.costBoxes(ctx.OUTPUTS)
  def INPUTS: CostedColl[Box] = dsl.costBoxes(ctx.INPUTS)
  def HEIGHT: Costed[Int] = {
    val cost = dsl.CostModel.SelectField
    new CCostedPrim(ctx.HEIGHT, cost, 4L)
  }
  def SELF: CostedBox = new CCostedBox(ctx.SELF, dsl.CostModel.AccessBox)
  def LastBlockUtxoRootHash: Costed[AvlTree] = {
    val tree = ctx.LastBlockUtxoRootHash
    new CCostedPrim(tree, dsl.CostModel.AccessAvlTree, tree.dataSize)
  }
  def minerPubKey: CostedColl[Byte] = dsl.costColWithConstSizedItem(ctx.minerPubKey, dsl.CostModel.PubKeySize.toInt, 1)
  def getVar[T](id: Byte)(implicit cT: RType[T]): CostedOption[T] = {
    val opt = ctx.getVar(id)(cT)
    dsl.costOption(opt, dsl.CostModel.GetVar)
  }

  def value = ctx
  def cost = ctx.cost
  def dataSize = ctx.dataSize

  def selfBoxIndex: Costed[Int] = {
    val cost = dsl.CostModel.SelectField
    new CCostedPrim(ctx.selfBoxIndex, cost, 4L)
  }

  @NeverInline
  def headers: CostedColl[Header] = SpecialPredef.rewritableMethod

  @NeverInline
  def preHeader: Costed[PreHeader] = SpecialPredef.rewritableMethod
}

class CCostedBox(val box: Box, val cost: Int) extends CostedBox {
  def dsl: SigmaDslBuilder = new TestSigmaDslBuilder
  def id: CostedColl[Byte] = dsl.costColWithConstSizedItem(box.id, box.id.length, 1)
  def valueCosted: Costed[Long] = {
    val cost = dsl.CostModel.SelectField
    new CCostedPrim(box.value, cost, 8L)
  }
  def bytes: CostedColl[Byte] = dsl.costColWithConstSizedItem(box.bytes, box.bytes.length, 1)
  def bytesWithoutRef: CostedColl[Byte] = dsl.costColWithConstSizedItem(box.bytesWithoutRef, box.bytesWithoutRef.length, 1)
  def propositionBytes: CostedColl[Byte] = dsl.costColWithConstSizedItem(box.propositionBytes, box.propositionBytes.length, 1)
  def registers: CostedColl[AnyValue] = {
    val len = box.registers.length
    val costs = dsl.Colls.replicate(len, dsl.CostModel.AccessBox)
    val sizes = box.registers.map(o => o.dataSize)
    new CCostedColl(box.registers, costs, sizes, dsl.CostModel.CollectionConst)
  }
  def getReg[@Reified T](id: Int)(implicit cT:RType[T]): CostedOption[T] = {
    val opt = box.getReg(id)(cT)
    dsl.costOption(opt, dsl.CostModel.GetRegister)
  }

  @NeverInline
  def creationInfo: Costed[(Int, Coll[Byte])] = SpecialPredef.rewritableMethod

  def value: Box = box
  def dataSize: Long = box.dataSize
}

//class CCostedAvlTree(val tree: AvlTree, val cost: Int) extends CostedAvlTree {
//  def dsl: SigmaDslBuilder = new TestSigmaDslBuilder
//  def startingDigest: CostedColl[Byte] = dsl.costColWithConstSizedItem(tree.digest, dsl.CostModel.PubKeySize.toInt, 1)
//  def enabledOperations: Costed[Byte] = new CCostedPrim(tree.enabledOperations, dsl.CostModel.SelectField, 1)
//  def keyLength: Costed[Int] = new CCostedPrim(tree.keyLength, dsl.CostModel.SelectField, 4)
//  def valueLengthOpt: CostedOption[Int] = dsl.costOption(tree.valueLengthOpt, dsl.CostModel.SelectField)
//
//  def value = tree
//  def dataSize = tree.dataSize
//}


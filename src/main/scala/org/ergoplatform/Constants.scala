package org.ergoplatform

import sigmastate.lang.exceptions.InterpreterException

sealed trait ConstantStatus

case object EnabledConstant extends ConstantStatus

case object DisabledConstant extends ConstantStatus

case class ReplacedConstant(newConstantId: Short) extends ConstantStatus

case class ChangedConstant(newValue: Array[Byte]) extends ConstantStatus

abstract class AbstractConstant[T] {
  def get(vs: ValidationSettings): Option[T]
  def isSoftFork(vs: ValidationSettings, constantId: Short, status: ConstantStatus, args: Seq[Any]): Boolean = false
}

case class MandatoryConstant[T](id: Short, description: String) extends AbstractConstant[T] {
  override def get(vs: ValidationSettings): Option[T] = {
    val status = vs.getConstantStatus(this.id)
    status match {
      case None =>
        throw new InterpreterException(s"Constant $this not found in validation settings")
      case Some(DisabledConstant) =>
        throw new MandatoryConstantRemovalException(vs, this)
      case Some(status) =>
        vs.getConstant(this.id).get._1()
    }
  }
}

case class ValidationConstant[T](id: Short, description: String) extends AbstractConstant[T] {
  override def get(vs: ValidationSettings): Option[T] = {
    val status = vs.getConstantStatus(this.id)
    status match {
      case None =>
        throw new InterpreterException(s"Constant $this not found in validation settings")
      case Some(DisabledConstant) =>
        None
      case Some(status) =>
        vs.getConstant(this.id).get._1()
    }
  }
}


case class MandatoryConstantRemovalException(vs: ValidationSettings, removedConstant: MandatoryConstant[_])
  extends SoftForkException(s"Mandatory constant ${removedConstant.id} was removed")
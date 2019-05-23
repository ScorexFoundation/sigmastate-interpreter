package org.ergoplatform

import sigmastate.lang.exceptions.InterpreterException

sealed trait ConstantStatus

case object EnabledConstant extends ConstantStatus

case object DisabledConstant extends ConstantStatus

case class ReplacedConstant(newConstantId: Short) extends ConstantStatus

case class ChangedConstant(newValue: Array[Byte]) extends ConstantStatus

abstract class AbstractConstant {
  protected def validate[T](vs: ValidationSettings, block: => T): T
  def getId: Short
  def isSoftFork(vs: ValidationSettings, constantId: Short, status: ConstantStatus, args: Seq[Any]): Boolean = false
}

case class MandatoryConstant(id: Short, description: String) extends AbstractConstant {
  override def validate[T](vs: ValidationSettings, block: => T): T = {
    val status = vs.getMandatoryConstantStatus(this.id)
    status match {
      case None =>
        throw new InterpreterException(s"Constant $this not found in validation settings")
      case Some(DisabledConstant) =>
        throw new MandatoryConstantRemovalException(vs, this)
      case Some(status) =>
        block
    }
  }

  override def getId: Short = id
}

case class ValidationConstant(id: Short, description: String) extends AbstractConstant {
  override def validate[T](vs: ValidationSettings, block: => T): T = {
    val status = vs.getConstantStatus(this.id)
    status match {
      case None =>
        throw new InterpreterException(s"Constant $this not found in validation settings")
      case Some(DisabledConstant) =>
        block
    }
  }

  override def getId: Short = id
}


case class MandatoryConstantRemovalException(vs: ValidationSettings, removedConstant: MandatoryConstant)
  extends SoftForkException(s"Mandatory constant ${removedConstant.id} was removed")


object ErgoConstants {
  object MaxBoxSizeConstant extends MandatoryConstant(2000,
  "Box size shouldn't exceed specified value") {
    def apply[T](vs: ValidationSettings): Long = {
      validate(vs, {
        64 * 1024
      })
    }
  }

  object MaxTokensConstant extends MandatoryConstant(2001,
  "Amount of tokens in the box") {
    def apply[T](vs: ValidationSettings): Byte = {
      validate(vs, {
        4.toByte
      })
    }
  }

  val constantSpecs: Seq[ValidationConstant] = Seq(
/*
    MaxBoxSizeConstant,
*/
  )
  val mandatoryConstantSpecs: Seq[MandatoryConstant] = Seq(
    MaxBoxSizeConstant,
    MaxTokensConstant,
  )
}
package sigma.js

import scorex.util.encode.Base16
import sigma.Extensions.{ArrayOps, TryOps}
import sigma.data.Iso.{isoStringToArray, isoStringToColl}
import sigma.data.{AvlTreeData, AvlTreeFlags, CAvlTree, Iso}
import sigmastate.fleetSdkCommon.distEsmTypesCommonMod.HexString

import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scala.scalajs.js.annotation.JSExportTopLevel

/** Equivalent of [[sigma.AvlTree]] available from JS. */
@JSExportTopLevel("AvlTree")
class AvlTree(
    val digest: String,
    val insertAllowed: Boolean,
    val updateAllowed: Boolean,
    val removeAllowed: Boolean,
    val keyLength: Int,
    val valueLengthOpt: UndefOr[Int]
) extends js.Object

@JSExportTopLevel("AvlTree$")
object AvlTree extends js.Object {

  /** Creates [[AvlTree]] with the given digest and all operations enabled. */
  def fromDigest(digestHex: HexString): AvlTree = {
    val digestBytes = Base16.decode(digestHex).getOrThrow.toColl
    val treeData = AvlTreeData.avlTreeFromDigest(digestBytes)
    isoAvlTree.from(CAvlTree(treeData))
  }

  implicit val isoAvlTree: Iso[AvlTree, sigma.AvlTree] = new Iso[AvlTree, sigma.AvlTree] {
    override def to(x: AvlTree): sigma.AvlTree = {
      CAvlTree(
        AvlTreeData(
          digest = isoStringToArray.to(x.digest).toColl,
          treeFlags = AvlTreeFlags(x.insertAllowed, x.updateAllowed, x.removeAllowed),
          x.keyLength,
          valueLengthOpt = sigma.js.Isos.isoUndefOr(Iso.identityIso[Int]).to(x.valueLengthOpt),
        ),
      )
    }

    override def from(x: sigma.AvlTree): AvlTree = {
      val tree = x.asInstanceOf[CAvlTree]
      val data = tree.treeData
      new AvlTree(
        digest = isoStringToColl.from(tree.digest),
        insertAllowed = data.treeFlags.insertAllowed,
        updateAllowed = data.treeFlags.updateAllowed,
        removeAllowed = data.treeFlags.removeAllowed,
        keyLength = data.keyLength,
        valueLengthOpt = sigma.js.Isos.isoUndefOr(Iso.identityIso[Int]).from(data.valueLengthOpt),
      )
    }
  }

}
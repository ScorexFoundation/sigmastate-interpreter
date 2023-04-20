package sigmastate

package object crypto {
  /** Instance of Elliptic Curve descriptor. */
  type Curve = Platform.Curve

  /** Instance of Elliptic Curve point. */
  type Ecp = Platform.Ecp

  /** Instance of Elliptic Curve field element. */
  type ECFieldElem = Platform.ECFieldElem
}

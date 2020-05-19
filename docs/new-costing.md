## Costing Rules

### Rule: EQ 

```
if (node.tpe.isConstantSize)  // (Boolean, Byte, etc)
  E.addCostOf(node)
else { // depend of actual data values l and r
  val lds = Sized.dataSizeOf[S](l, left.tpe)
  val rds = Sized.dataSizeOf[S](r, right.tpe)
  E.addPerKbCostOf(node, lds + rds)
}
```

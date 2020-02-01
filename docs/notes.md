## Costing

### EQ cost

```
P: ConstantSize type (Boolean, Byte, etc)
def costOf(node: Value): Int

EQ
---
C[P](x: P, y: P) = costOf(EQ)
C[(A,B)](x: (A,B), y: (A,B)) = C(x._1, y._1) + C(x._2, y._2)  
C[Coll[P]](x: Coll[P], y: Coll[P]) = 

```

## Approximate sizes of different dependencies

These dependencies can be removed with simple refactoring

| Jar           | Size, Kb  |
|---------------|---------------|
| kiama_2.12-2.1.0.jar  |  652          |
| - jline-2.14.3.jar  |  268          |
| - scallop_2.12-2.1.1.jar  |  348          |
| - dsprofile_2.12-0.4.0.jar  |  49          |
| - dsprofile_2.12-0.4.0.jar  |  49          |
| - dsinfo_2.12-0.4.0.jar  |  15       |
| - jsr305-3.0.2.jar  |  20        |
| com.typesafe.config:config-1.3.3.jar  |  286     |
| commons-io-2.5.jar  |  209          |
| cats-core_2.12-1.4.0.jar  |  4400   |
| - cats-kernel_2.12-1.4.0.jar  |  3200   |
| spire_2.12-0.14.1.jar  |  7700   |
| - algebra_2.12-0.7.0.jar  |  1100   |
| - spire-macros_2.12-0.14.1.jar | 73 |





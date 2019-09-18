## Scala Performance Style Guide 

### Motivation

Scala is high-level language with powerful constructs and idioms to
create abstractions which allow to achieve code correctness and clarity.

However, many abstractions come with performance penalty to be payed at runtime.
Meanwhile, a given task can often be solved in many different ways. 
And it turns out that for many constructions there exists much faster alternative
which, at the same time, have comparable abstraction level and code clarity.

This document is a collection of such alternatives. The recommendations can be used
in addition to the classical Style Guide [1], these recipes are recommendations only, 
there are always exceptions.

This guide can be used by code writers and reviewers to reason about code quality from
performance point of view.
This is particularly important in those 20% of code which is known to be a hotspot or
hotspot candidate.

I’d like to point out that performance measurements referred in this guide are only to 
highlight what’s possible to expect, and not to produce exact numbers. 
Some optimizations can be easily measured, while others are not. Some measurements 
might be distorted in real world code by other optimizations that are not taken into 
account in the micro-benchmarks. 

### Empty Seq

It is quite often to see `Seq()` where empty collection is required.
However, this means the following method from `GenericCompanion` is called

```scala
def apply[A](elems: A*): CC[A] = {
    if (elems.isEmpty) empty[A]
    else {
      val b = newBuilder[A]
      b ++= elems
      b.result()
    }
}
```

##### What to use instead

Simple `Nil` is 3-20x faster depending on the context 
(see  performance of "Seq" in `BasicBenchmarks.scala`)

### Empty Map

It is quite often to see `Map()` where empty Map is required.
However, this means the following method from `GenMapFactory` is called

```scala
def apply[A, B](elems: (A, B)*): CC[A, B] = (newBuilder[A, B] ++= elems).result()
```
##### What to use instead

Calling `Map.empty` is 50-70% faster depending on the context 
(see  performance of "Map" in `BasicBenchmarks.scala`)


### Looping using `for`

Looping pattern `for (x <- xs) { ... }` is used quite often due to it's convenience.
It looks like `x is bound to each element and block of code is executed`.
However it is desugared to `xs.foreach { x => ... }` which, besides
execution of the block of code involves the following overhead points:
1) `foreach` method call
2) allocation of lambda object
3) boxing of lambda argument for every xs item (if xs values are not yet boxed)
4) hidden overhead of concrete foreach implementation

##### What to use instead

The following code is recommended replacement if xs provides O(1) indexing operation,
especially if `xs` is `Array` wrapped into `Seq`.

```scala
import spire.syntax.all.cfor
cfor(0)(_ < xs.length, _ + 1) { i => 
  val x = xs(i)
      ...
}
```

Here `cfor` is a macros from [spire](https://github.com/non/spire) library.
This is compiled to efficient Java `for` loop and avoids overhead points 1) - 4).
Depending on xs.length it is 20-50x faster (see `BasicBenchmark.scala`).
And since `foreach` already implies a side effect operation `cfor` doesn't make 
the code less readable.

### Creating Sequences with Seq(...)

It is tempting to use `Seq.apply` method where a Seq of items is required like 
`Seq(1, 2, 3)` because it is easy and concise. You can pass it as method argument 
or as method result.
However, the following happens under the hood:
1) new Array with 1, 2, 3 items is created, and each item is boxed into its own Integer object
2) WrappedArray#ofInt wrapper created to pass it as vararg argument of `Seq.apply`
3) the above mentioned method from `GenericCompanion` is called.
4) new ListBuffer created and all items are copied from array to the buffer

All this would be executed by JVM interpreter thousands of time before compilation
threshold is reached and HotSpot optimizer hopefully optimizes it away.

##### What to use instead

Simple drop-in replacement `Array(1, 2, 3)` would do the job.
The benchmark shows that this code is 4-10x faster than using `Seq(...)`.
Even if `tail` method on the created sequence is used, it is still 3x faster.
(see  performance of "Seq vs Array" in `BasicBenchmarks.scala`).

What happens here is that 1) native unboxed Java array is created and then
2) wrapped via implicit conversion `wrapIntArray` into `WrappedArray#ofInt` 
object which is inherited from `Seq` trait so it can be used directly. 

Why this is faster:
1) Avoid additional allocations (vs only two new objects are allocated in Array case). 
Note that not only each Int is boxed to java.lang.Integer (or other primitive type), 
but also `scala.collection.immutable.::` instances are created for each item.
2) Avoid boxing which is proportional to the size of Seq
3) The access to array items is cache local both when the array is created and 
when it is later used
4) Less allocations means less garbage to collect later. This is especially 
important when the application is multi-threaded, because in this case garbage 
collector will compete with application threads for CPU resources, thus further
slowing down computations.

This seems like universal acceleration recipe. It doesn't make your code less readable.
The arrays created in this way are only accessible via `Seq` interface, 
which is immutable. Thus, better performance is achieved without sacrificing other
desirable code qualities like readability, safety, conciseness.

### Abstract class vs trait

Traits are more flexible than classes because they can be mixed in various ways. 
That is often convenient when the code is prototyped and when the code shape is 
still changing. Also they are easier to type. 

However traits come with inherent performance penalty. Invocation of trait's method
results in `invokeinterface` opcode of JVM instead of `invokevirtual` opcode for 
class method invocation. First, HotSpot JIT needs to do more work to optimize 
execution of `invokeinterface`, which is not always possible. Second, if it fails 
to optimize, then every call will have an additional method search before an actual 
invocation takes place.

##### What to use instead

When prototyping use `abstract class` instead of `trait` declaration by default.
This way you avoid over generalization of you type hierarchy, and you will have 
better understanding of where you really need traits and why.

Try changing `trait` declarations with `abstract class` declarations. In many cases
this is as simple as that, and you get performance gains almost for free.
This however not always possible with published APIs as it may break compatibility.


### Concluding remarks

Program performance is the result of everyday work, rather than one time job.
There is no *one size fits all* solution as there are many trade-offs along the way.
You may find it useful to examine the References section for more detailed information.

### References
1. [Scala Style Guide](https://docs.scala-lang.org/style/)
2. [Scala High Performance Programming](https://www.amazon.com/Scala-Performance-Programming-Vincent-Theron/dp/178646604X)
3. [Optimizing Higher-Order Functions in Scala](https://infoscience.epfl.ch/record/128135/files/paper.pdf) (somewhat outdated)
4. [Where to look first when optimizing Scala code?](https://stackoverflow.com/questions/15112604/where-to-look-first-when-optimizing-scala-code)
5. [Scala for comprehension performance](https://stackoverflow.com/questions/15137360/scala-for-comprehension-performance)
6. [Performance characteristics of Scala collections](https://docs.scala-lang.org/overviews/collections/performance-characteristics.html)
7. [Java Performance: The Definitive Guide: Getting the Most Out of Your Code](https://www.amazon.com/Java-Performance-Definitive-Guide-Getting/dp/1449358454)
8. [Scala library benchmarks](https://github.com/scala/scala/tree/2.13.x/test/benchmarks)
9. [JITWatch](https://github.com/AdoptOpenJDK/jitwatch)
10. [Parallel Collections: Measuring Performance](https://docs.scala-lang.org/overviews/parallel-collections/performance.html)
11. [JVM JIT optimization techniques](https://advancedweb.hu/2016/05/27/jvm_jit_optimization_techniques/)

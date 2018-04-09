# sigmastate-interpreter
Interpreter for a family of Sigma-State authentication languages

## Getting started

Because there is currently no published version of Sigma-state interpreter,
to use it in your project you first need to:

 1. Clone or download [sigmastate-interpreter](https://github.com/ScorexFoundation/sigmastate-interpreter) 
   (`git clone git@github.com:ScorexFoundation/sigmastate-interpreter.git` from command line).
 2. Run `sbt publishLocal` in the directory it was cloned to. 
 This will publish the artifacts in the local ivy repository (usually at `$HOME/.ivy`)
 3. In your own project add library dependency
 ```
libraryDependencies ++= Seq(
  "org.scorexplatform" %% "sigma-state" % "0.0.1"
) 
 ```


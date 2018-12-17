package sigmastate.utxo.benchmarks

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object BenchmarkUtils {
//  def measureTime[T](action: => T): (T, Long) = {
//    val t0 = System.currentTimeMillis()
//    val res = action
//    val t = System.currentTimeMillis()
//    (res, t - t0)
//  }
//
//  def runTasks(nTasks: Int)(block: Int => Unit) = {
//    val (_, total) = measureTime {
//      val tasks = (1 to nTasks).map(iTask => Future(block(iTask)))
//      val res = Await.result(Future.sequence(tasks), Duration.Inf)
//    }
//    println(s"Completed $nTasks tasks in $total msec")
//  }
//
//  def measure[T](nIters: Int, okShow: Boolean = true)(action: Int => Unit): Unit = {
//    for (i <- 0 until nIters) {
//      val (res, iterTime) = measureTime(action(i))
//      if (okShow)
//        println(s"Iter $i: $iterTime ms")
//    }
//  }
}

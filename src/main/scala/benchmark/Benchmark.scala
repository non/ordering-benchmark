package benchmark

import scala.{specialized => spec}
import scala.util.Random

import scala.math._
import scala.math.Ordering.Implicits._

import com.google.caliper.Runner 
import com.google.caliper.SimpleBenchmark

import scala.{specialized => spec}

object Util {
  def run(reps:Int)(f: =>Unit) = for(i <- 0 until reps)(f)

  // sugar for building arrays using a per-cell init function
  def init[A:Manifest](size:Int)(init: =>A) = {
    val data = Array.ofDim[A](size)
    for (i <- 0 until size) data(i) = init
    data
  }

  // handy aliases into Random
  def nextInt() = Random.nextInt()
  def nextLong() = Random.nextLong()
  def nextFloat() = Random.nextFloat()
  def nextDouble() = Random.nextDouble()

  // sugar for building randomized arrays of various types
  def initInts(size:Int) = init(size)(nextInt())
  def initLongs(size:Int) = init(size)(nextLong())
  def initFloats(size:Int) = init(size)(nextFloat())
  def initDoubles(size:Int) = init(size)(nextDouble())
}

// extend this to create an actual benchmarking class
trait MyBenchmark extends SimpleBenchmark {
}

// extend this to create a main object which will run 'cls' (a benchmark)
trait MyRunner {
  val cls:java.lang.Class[_ <: com.google.caliper.Benchmark]
  def main(args:Array[String]) {
    println("starting benchmarks...")
    Runner.main(cls, args:_*)
    println("completed benchmarks.")
  }
}


/**
 * AddBenchmarks
 *
 * Benchmarks the speed of direct/generic addition.
 */
object AddBenchmarks extends MyRunner { val cls = classOf[AddBenchmarks] }
class AddBenchmarks extends MyBenchmark {
  val size = 10 * 1000 * 1000
  val longs = Util.initLongs(size)
  val x = Int.MaxValue.toLong

  // ====================================

  def generic1CountLessThan[T:Ordering:Manifest](t:T, data:Array[T]) = {
    var i = 0
    var count = 0
    while (i < data.length) {
      if (data(i) < t) count += 1
      i += 1
    }
    count
  }

  def generic2CountLessThan[T](t:T, data:Array[T])(implicit o:Ordering[T], m:Manifest[T]) = {
    var i = 0
    var count = 0
    while (i < data.length) {
      if (o.lt(data(i), t)) count += 1
      i += 1
    }
    count
  }

  def directCountLessThan(t:Long, data:Array[Long]) = {
    var i = 0
    var count = 0
    while (i < data.length) {
      if (data(i) < t) count += 1
      i += 1
    }
    count
  }

  def timeGeneric1CountLessThan(reps:Int) = Util.run(reps)(generic1CountLessThan(x, longs))
  def timeGeneric2CountLessThan(reps:Int) = Util.run(reps)(generic2CountLessThan(x, longs))
  def timeDirectCountLessThan(reps:Int) = Util.run(reps)(directCountLessThan(x, longs))

  // ---------------------------------------

  def generic1CountEqualTo[T:Ordering:Manifest](t:T, data:Array[T]) = {
    var i = 0
    var count = 0
    while (i < data.length) {
      if (data(i) equiv t) count += 1
      i += 1
    }
    count
  }

  def generic2CountEqualTo[T](t:T, data:Array[T])(implicit o:Ordering[T], m:Manifest[T]) = {
    var i = 0
    var count = 0
    while (i < data.length) {
      if (o.equiv(data(i), t)) count += 1
      i += 1
    }
    count
  }

  def directCountEqualTo(t:Long, data:Array[Long]) = {
    var i = 0
    var count = 0
    while (i < data.length) {
      if (data(i) == t) count += 1
      i += 1
    }
    count
  }

  def timeGeneric1CountEqualTo(reps:Int) = Util.run(reps)(generic1CountEqualTo(x, longs))
  def timeGeneric2CountEqualTo(reps:Int) = Util.run(reps)(generic2CountEqualTo(x, longs))
  def timeDirectCountEqualTo(reps:Int) = Util.run(reps)(directCountEqualTo(x, longs))

  // ---------------------------------------

  def generic1FindMax[T:Ordering:Manifest](data:Array[T]) = {
    var i = 0
    var m = data(0)
    while (i < data.length) {
      m = m.max(data(i))
      i += 1
    }
    m
  }

  def generic2FindMax[T](data:Array[T])(implicit o:Ordering[T], m:Manifest[T]) = {
    var i = 0
    var m = data(0)
    while (i < data.length) {
      m = o.max(m, data(i))
      i += 1
    }
    m
  }

  def directFindMax(data:Array[Long]) = {
    var i = 0
    var m = data(0)
    while (i < data.length) {
      m = scala.math.max(m, (data(i)))
      i += 1
    }
    m
  }

  def timeGeneric1FindMax(reps:Int) = Util.run(reps)(generic1FindMax(longs))
  def timeGeneric2FindMax(reps:Int) = Util.run(reps)(generic2FindMax(longs))
  def timeDirectFindMax(reps:Int) = Util.run(reps)(directFindMax(longs))

  // ---------------------------------------
}

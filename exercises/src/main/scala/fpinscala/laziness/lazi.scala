package fpinscala.laziness

import fpinscala.laziness.Stream._

/**
  * Created by Giovanni on 7/17/2017.
  */
object lazi {
  def main(args: Array[String]): Unit = {
    val stream = Stream(1, 2, 3, 4)
    println(stream.toList)
    println(stream.take(3).toList)
    println(stream.drop(3).toList)
    println(stream.drop(2).toList)
    println(stream.drop(4).toList)
    println(empty.drop(4).toList)
    println(stream.takeWhile(_ < 3).toList)
    println(cons(
      {println(1); 1},
      cons({println(2); 2}, Empty))
    .takeWhileFold(_ < 2).toList)
    println(Empty.headOption)
    println(Stream(1, 2, 3).headOption)
    Stream(1, 2, 3, 4).map(_ + 1).filter(_ % 2 == 0).toList
    println("Map")
    println(cons(1, cons(4, cons({println("Hi"); 2}, empty))).map(_ * 2).take(1).toList)
    println("Filter")
    println(Stream(1, 3, {println("HI"); 3}).filter(_ == 3).take(1).toList)
    println("Append")
    println(Stream(1, 3, 3).append({print(3); 3}).take(1).toList)
    println("Concatenate")
    println(Stream(1, 3, 3).concatenate(Stream({print(3); 3})).take(1).toList)
    println("Flatmap")
    println(Stream(1, 2, 3).flatMap(a => Seq(a, {print(a * a); a * a})).take(2).toList)
    println("Find")
    println(cons(1, cons({print(3); 3}, empty)).find(_ == 1).toList)
    println("Constant")
    println(constant(5).take(3).toList)
    println("ones")
    println(ones.take(8).toList)
    println(ones.takeWhile(_ == 1).take(5).toList)
    println(ones.map(_ + 1).exists(_ % 2 == 0))
    println("from")
    println(from(10).take(12).toList)
    println("fib")
    println(fib(1, 0).take(8).toList)
    println("Unfold fib")
    println(fibUnfolded().take(15).toList)
    println("Unfold from")
    println(fromUnfolded(5).take(15).toList)
    println("Unfold constant")
    println(constantUnfolded(5).take(15).toList)
    println("Unfold ones")
    println(onesUnfolded().take(15).toList)
    println("Unfold map")
    println(mapUnfolded(ones)(_ * 2).take(15).toList)
    println("Unfold take")
    println(takeUnfolded(ones)(2).toList)
    println("Unfold takeWhile")
    println(takeWhileUnfolded(from(10))(_ < 15).toList)
    println("Unfold zipWith")
    println(from(15).zipWith(from(10))(_ - _).take(15).toList)
    println("Unfold zipAll")
    println(zipAllUnfolded(from(15).take(5), from(10).take(15)).take(15).toList)
    println("startsWith")
    println(from(5).startsWith(from(4).take(3)))
    println("tails")
    println(from(4).take(3).tails.exists(_ startsWith(Stream())))

  }
}

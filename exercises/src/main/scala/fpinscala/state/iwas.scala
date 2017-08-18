package fpinscala.state

import fpinscala.state.State

object iwas {
  def main(args: Array[String]): Unit = {
    val rng = RNG.Simple(42)
    println(RNG.nonNegativeInt(rng)._1)
    println(RNG.double(rng)._1)
    println(RNG.ints(5)(rng))
    println(RNG.doubleMap(rng))
    println("map2")
    println(RNG.map2(RNG.double, RNG.double)(_ + _)(rng))
    println("sequence")
    println(RNG.sequence(List(RNG.doubleMap, RNG.doubleMap, RNG.doubleMap))(rng))
    println("intsSequence")
    println(RNG.intsSequence(5)(rng))

  }

}

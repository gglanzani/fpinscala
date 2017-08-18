package fpinscala.errorhandling

import fpinscala.errorhandling.Option._

object error {


  def main(args: Array[String]): Unit = {
    val some = Some(5)
    val none = None
    println(some.map(_ * 2))
    def testFlatMap(op: Option[Int], f: Int => Option[Int]): Option[Int] = {
      op.flatMap(f)
    }
    println(testFlatMap(some, (x) => x match {
      case 0 => None
      case _ => Some(5 / x)}))
    println(testFlatMap(Some(0), (x) => x match {
      case 0 => None
      case _ => Some(5 / x)}))
    println(variance(List(1, 2, 3)))
    println(map2(Some(1), Some(2))(_ + _))
    println(sequence(List(Some(2), Some(3), None)))
    println(sequence(List(Some(2), Some(3), Some(0))))
    println(combine(Some(1), Some(List(1, 2, 3))))
    println(combine(None, Some(List(1, 2, 3))))
    println(combine(None, None))
    println(combine(Some(1), None))
    println(_traverse(Some(List(1, 2, 0, 5)))(
      (x) => x match {
      case 0 => None
      case _ => Some(5 / x)}
    ))
    println(_traverse(Some(List(1, 2, 3, 5)))(
      (x) => x match {
        case 0 => None
        case _ => Some(5 / x)}
    ))
  }
}

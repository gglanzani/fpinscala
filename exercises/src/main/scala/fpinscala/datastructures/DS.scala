package fpinscala.datastructures

//import fpinscala.datastructures.List

object DS {

  def main(args: Array[String]): Unit = {
    val xs = List(1, 2, 3, 4)
    println(List.dropWhile(xs, (x: Int) => x < 3))
    println(List.init(xs))
    println(List.init(Nil))
    println(List.foldLength(xs))
    println(List.foldLeft(xs, 0)(_ + _))
    println(List.length3(xs))
    println(List.reverse(xs))
    println(List.append(xs, 5))
    println(List.concatenate(xs, xs))
    println(List.filter(xs)(_ % 2 == 0))
    println(List.flatMap(xs)(i => List(i, i)))
    println(List.flatFilter(xs)(_ % 2 == 0))
    println(List.addLists(xs, List.append(xs, 5)))
    println(List.zipWith(xs, List.append(xs, 5))(_ * _))
    println(List.hasSubsequence(xs, List(2, 3, 5)))
    println(List.hasSubsequence(xs, List(1, 3, 5)))
    println(List.hasSubsequence(xs, List(2, 3, 4)))
    println(List.hasSubsequence(xs, List(4)))

    val t = Branch(Leaf(1), Leaf(2))
    val t1 = Branch(t, t)
    val t2 = Branch(t, Leaf(0))
    println(Tree._size(t, 0))
    println(Tree._size(t1, 0))
    println(Tree._size(Leaf(1), 0))
    println(Tree.maximum(t1))
    println(Tree.depth(t1))
    println(Tree.depth(t))
    println(Tree.depth(t2))
    println(Tree.depth(t2))
    println(Tree.map(t2)(_ + 1))
    println(Tree.newMaximum(t2))
    println(Tree.newDepth(Leaf(1)))
  }
}

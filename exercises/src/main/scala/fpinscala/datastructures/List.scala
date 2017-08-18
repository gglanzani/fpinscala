package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else drop(tail(l), n - 1)
  }

  def head[A](l: List[A]): A = l match {
    case Cons(h, _) => h
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    if (f(head(l))) dropWhile(tail(l), f)
    else l
  }


  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    }


  def length[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(h, t) => 1 + length(t)
  }

  def foldLength[A](l: List[A]): Int = {
    foldRight(l, 0)((x, z) => 1 + z)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)

  def product3(xs: List[Double]): Double = foldLeft(xs, 1.0)(_ * _)

  def length3[A](xs: List[A]): Int = foldLeft(xs, 0)((z, _) => z + 1)

  def reverse[A](xs: List[A]): List[A] = foldLeft(xs, Nil:List[A])((z, x) => Cons(x, z))

  def foldrRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((a, z) => f(z, a))

  def append[A](as: List[A], a: A): List[A] = foldrRight(as, List(a))(Cons(_, _))

  def concatenate[A](as: List[A], bs: List[A]): List[A] = foldrRight(as, bs)(Cons(_, _))

  def addOne(as: List[Int]): List[Int] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, addOne(t))
  }

  def stringify(as: List[Double]): List[String] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(h.toString, stringify(t))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(l)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
    case Cons(h, t) => filter(t)(f)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => concatenate(f(h), flatMap(t)(f))
  }

  def flatFilter[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => f(a) match {
      case true => List(a)
      case _    => Nil
    })
  }

  def addLists(as: List[Int], bs: List[Int]): List[Int] = as match {
    case Nil => bs match {
      case Nil => Nil
      case _ => bs
    }
    case Cons(ah, at) => bs match {
      case Nil => Nil
      case Cons(bh, bt) => Cons(ah + bh, addLists(at, bt))
    }
  }

    def zipWith[A,B](as: List[A], bs: List[B])(f: (A, B) => B): List[B] = as match {
      case Nil => bs match {
        case Nil => Nil
        case _ => bs
      }
      case Cons(ah, at) => bs match {
        case Nil => Nil
        case Cons(bh, bt) => Cons(f(ah, bh), zipWith(at, bt)(f))
      }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def _hasSubsequence[A](sup: List[A], sub: List[A], sentinel: Boolean): Boolean = sub match {
      case Nil => sentinel
      case Cons(hsub, tsub) => sup match {
        case Nil => false
        case Cons(h, t) if h == hsub => _hasSubsequence(t, tsub, true)
        case _ => _hasSubsequence(tail(sup), sub, false)
      }
    }
    _hasSubsequence(sup, sub, false)
  }

}

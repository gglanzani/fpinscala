package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = {
    this match {
      case Empty => Nil
      case Cons(h, t) => h()::(t().toList)
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
      case _ if n == 0 => Empty
      case Empty => Empty
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 0 => t().drop(n - 1)
      case a if n == 0 => a
      case Empty => empty
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
      case _ => empty
    }
  }

  def takeWhileFold(p: A => Boolean): Stream[A] = {
    foldRight[Stream[A]](Empty)((a, b) => if (p(a)) {
      cons(a, b)
    } else {
      empty
    })
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight[Option[A]](None)((a, b) => Some(a))

  def map[B](f: (=> A) =>  B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, b) => if (p(a)) cons(a, b)
                                          else b)

  def append[B>:A](el: => B): Stream[B] =
    foldRight(Stream(el))((a, b) => cons(a, b))

  def concatenate[B>:A](bs: => Stream[B]): Stream[B] =
    foldRight(bs)((a, b) => cons(a, b))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def flatMap[B](f: A => Seq[B]): Stream[B] =
    foldRight[Stream[B]](Empty)((a, b) => Stream(f(a):_*).concatenate(b))

  def zipWith[B,C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, bs))( {case (a, b) => (a, b) match {
      case (Cons(ha, ta), Cons(hb, tb)) => Some((f(ha(), hb()), (ta(), tb())))
      case _                            => None
    }
    } )

  def startsWith[B>:A](other: Stream[B]): Boolean =
    this.zipWith(other)(_ == _).forAll(_ == true)

  def tails: Stream[Stream[A]] = unfold(this)(z => z match {
    case Cons(h, t) => Some( (z, t()) )
    case _ => None
  })

  def hasSubsequence[A](other: Stream[A]): Boolean =
    this.tails.exists(_ startsWith other)
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))
  def fib(a: Int, b: Int): Stream[Int] = Stream.cons(b, fib(b, a + b))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some((a, zo)) => Stream.cons(a, unfold(zo)(f))
  }

  def fibUnfolded(): Stream[Int] = unfold((0, 1))({case (z1, z2) => Some((z1, (z2, z1 + z2)))})
  def fromUnfolded(n: Int): Stream[Int] = unfold(n)(z => Some(z, z + 1))
  def constantUnfolded[A](a: A): Stream[A] = unfold(a)(z => Some(z, z))
  def onesUnfolded(): Stream[Int] = unfold(1)(z => Some(z, z))

  def mapUnfolded[A,B](a: Stream[A])(f: A => B): Stream[B] = unfold(a)(z => z match {
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  })

  def takeUnfolded[A](a: Stream[A])(n: Int): Stream[A] = unfold(a)(z => z match {
    case Cons(h, t) if n > 0 => Some( (h(), takeUnfolded(t())(n - 1)) )
    case _ => None
  })

  def takeWhileUnfolded[A](a: Stream[A])(p: A => Boolean): Stream[A] = unfold(a)(z => z match {
    case Cons(h, t) if p(h()) => Some((h(), takeWhileUnfolded(t())(p)))
    case _ => None
  })

  def zipWithUnfolded[A,B,C](as: Stream[A], bs: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((as, bs))( {case (a, b) => (a, b) match {
      case (Cons(ha, ta), Cons(hb, tb)) => Some((f(ha(), hb()), (ta(), tb())))
      case _                            => None
    }
    } )

  def zipAllUnfolded[A,B](as: Stream[A], bs: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(as, bs)( {case (a, b) => (a, b) match {
      case (Cons(ha, ta), Cons(hb, tb)) => Some( (Some(ha()), Some(hb())), (ta(), tb()))
      case (Cons(ha, ta), _)            => Some( (Some(ha()), None), (ta(), Empty))
      case (_, Cons(hb, tb))            => Some( (None, Some(hb())), (Empty, tb()))
      case _                            => None
    }})
}
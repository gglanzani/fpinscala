package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Right(a) => Right(f(a))
   case Left(l) => Left(l)
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Left(l) => Left(l)
   case Right(a) => f(a)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Left(l) => b
   case Right(r) => Right(r)
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Right(r), Right(br)) => Right(f(r, br))
    case (_, Left(e)) => Left(e)
    case (Left(e), _) => Left(e)
  }

}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def combine[E, A](a: Either[E, A], as: Either[E, List[A]]): Either[E, List[A]] = (a, as) match {
    case (Left(e), _) => Left(e)
    case (Right(r), Right(rs)) => Right(r::rs)
  }

  def _traverse[E,A,B](es: Either[E, List[A]])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Right(Nil) => Right(Nil)
    case Left(e) => Left(e)
    case Right(i::is) => f(i) match {
      case Left(e) => Left(e)
      case Right(r) => combine(Right(r), _traverse(Right(is))(f))
    }
  }

  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = _traverse(Right(es))(f)

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = traverse(es)(x => x)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}
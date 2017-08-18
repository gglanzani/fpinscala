package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {


  def _size[A](t: Tree[A], acc: Int): Int = t match {
    case Leaf(_) => (1 + acc)
    case Branch(l, r) => ( _size(l, 0) + (1 + acc)) + _size(r, 0)
  }


  def maximum(t: Tree[Int]): Int = {
    def _maximum(t: Tree[Int], acc: Int)(f: (Int, Int) => Int): Int = t match {
      case Leaf(a) => f(a, acc)
      case Branch(l, r) => _maximum(r, _maximum(l, acc)(f))(f)
    }

    _maximum(t, Int.MinValue)((a: Int, acc: Int) => a max acc)
  }

  def depth[A](t: Tree[A]): Int = {
    def _depth[A](t: Tree[A], acc: Int): Int = t match {
      case Leaf(_) => acc + 1
      case Branch(l, r) => _depth(l, acc + 1) max _depth(r, acc + 1)
    }

    _depth(t, 0)
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A], acc: B)(fLeaf: (A, B) => B)(fBranch: (B, B, B) => B): B = t match {
    case Leaf(a) => fLeaf(a, acc)
    case Branch(l, r) => fBranch(fold(l, acc)(fLeaf)(fBranch), fold(r, acc)(fLeaf)(fBranch), acc)
  }


  def newsize[A](t: Tree[A]): Int = fold(t, 0)((_, acc) => 1 + acc)((l, r, acc) => l + r + acc + 1)

  def newMaximum(t: Tree[Int]): Int = fold(t, Int.MinValue)(_ max _)(_ max _ max _)

  def newDepth[A](t: Tree[A]): Int = fold(t, 0)((_, acc) => acc + 1)((l, r, acc) => (l max r) + 1 + acc)

}
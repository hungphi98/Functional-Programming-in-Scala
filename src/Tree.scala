/**
  * Created by Phi on 12/20/18.
  */

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def fold[A, B](as: Tree[A])(f: A => B)(g: (B, B) => B): B = as match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)((a) => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)((a) => 0)((t1, t2) => 1 + (t1 max t2))

  def maxViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)((a) => Leaf(f(a)): Tree[B])(Branch(_, _))

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 0
    case Branch(l, r) => 1 + depth(l) max 1 + depth(r)
  }

  def map[A](as: Tree[A])(f: A => A): Tree[A] = as match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }


  def main(args: Array[String]) {

    println(size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))))
    println(maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))))
    println(depth(Branch(Branch(Branch(Leaf(0), Branch(Leaf(8), Leaf(9))), Leaf(2)), Leaf(3))))
    println(map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(_ + 1))
  }
}

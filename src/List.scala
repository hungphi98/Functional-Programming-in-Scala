/**
  * Created by Phi on 12/18/18.
  */
package fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B) : B = as match {
    case Nil => z
    case Cons(x,xs) => f(x, foldRight(xs,z)(f))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns,0)((x,y) => x + y)

  def product2(ns : List[Double]) =
    foldRight(ns,1.0)(_ * _)

  def length[A](as : List[A]) : Int =
    foldRight(as, 0)((x,y)=>1+y)



  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail(as : List[Any]) : Any = as match{
    case Nil => Nil
    case Cons(x,xs) => xs
  }

  def setHead(as : List[Any], element : Any) : Any = as match{
    case Nil => Nil
    case Cons(x,xs) => Cons(element,xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match{
    case Cons(h,t) if (n>0) => drop(t,n-1)
    case _ => l
  }

  def dropWhile[A] (as : List[A])(f: A => Boolean) : List[A] = as match{
    case Cons(h,t) if f(h) => dropWhile(t)(f)
    case _ => as
  }

  def append[A](a1: List[A], a2: List[A]) : List[A] = a1 match{
    case Nil => a2
    case Cons(h,t) => Cons(h,append(t,a2))
  }

  def main(args: Array[String]) {
    var x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    printf("%d\n",x)
    println(apply(x))
    println(tail(List(1,2,3,4,5)))
    println(setHead(List(1,2,3,4,5),10))
    println(drop(List(1,2,3,4,5,6,7,8),4))
    println(length(List(1,2,3,4)))
  }
}

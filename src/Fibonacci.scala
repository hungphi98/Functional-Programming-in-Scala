/**
  * Created by Phi on 11/30/18.
  */
object Fibonacci  {
  def fibonacci(n: Int) : Int = {
    def go(n: Int, a: Int, b : Int): Int =
      if (n <= 0) a
      else go(n-1,b,a+b)
    go(n,0,1)
  }

  def findFirst(ss : Array[String], key: String) : Int = {
    @annotation.tailrec
    def loop(n: Int) : Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n+1)
    loop(0)
  }

  def findFirst[A](ss: Array[A], p : A => Boolean) : Int = {
    def loop(n: Int) : Int =
      if (n >= ss.length) -1
      else if (p(ss(n))) n
      else loop(n+1)
    loop(0)
  }

  def isSorted[A] (ss : Array[A],  ordered: (A,A) => Boolean) : Boolean = {
    def loop(n: Int) : Boolean =
      if (n >= ss.length) true
      else if (!ordered(ss(n),ss(n+1))) false
      else loop(n+1)
    loop(0)
  }

  def main(args: Array[String]) {
    println(fibonacci(5))
    var z = Array("Uno", "Dos", "Tres")
    println(findFirst(z,"Uno"))
    println(findFirst(z, "Dos"))
    println(Array(1,2,3,4,5))
  }

}

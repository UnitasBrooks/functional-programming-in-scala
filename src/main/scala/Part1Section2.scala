object Part1Section2 {
  // Exercise 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(lastTwo: (Int, Int), current: Int): Int = {
      if (current > n) lastTwo._1
      else {
        val nextTwo: (Int, Int) = (lastTwo._2, lastTwo._1 + lastTwo._2)
        go(nextTwo, current + 1)
      }
    }
    go((0,1), 1)
  }

  // Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int, lastA: A): Boolean = {
      if (n >= as.length) true
      else if (!ordered(as(n), lastA)) false
      else loop(n + 1, as(n))
    }
    loop(0, as(0))
  }

  // Exercise 2.3
  def curry[A,B,C](f: (A, B) => C): A => B => C = {
    a: A =>  b: B => f(a, b)
  }

  // Exercise 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // Exercise 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = { a: A => f(g(a)) }
}
object Part1Section2Notes {
  def formatResult(name: String, number: Int, mathFunction: Int => Int): String = {
    val msg = s"The $name of $number is ${mathFunction(number)}"
    msg
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = { if (n <= 0) acc else go(n - 1, n*acc) }
    go(n, 1)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    loop(0)
  }

  def curryExample[A,B,C](a: A, f: (A,B) => C): B => C = { b => f(a, b) }
}

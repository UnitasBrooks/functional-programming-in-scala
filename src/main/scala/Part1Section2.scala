object Part1Section2 {
  def main(args: Array[String]): Unit = {
    var number: Int = -1
    try {
      number = args(0).toInt
    }
    catch {
      case _: java.lang.ArrayIndexOutOfBoundsException => number = 10
      case _: java.lang.NumberFormatException =>
        println("Need to provide an integer value, not a string.")
        System.exit(1)
    }
    println(formatResult("factorial", number, factorial))
    println(formatResult("fibonacci sequence", number, fib))
    println(findFirst(Array(1, 2, 2, 3, 5, 4, 6, number), (a: Int) => a == number))
    println(isSorted(Array(1, 2, 2, 3, 5, 5, 6), (a: Int, b: Int) => a >= b))
    println(curryExample(number, (i: Int, s: String) => { s.length.toFloat / i.toFloat })("Hello world!"))

    val concatIntsFunction = (a: Int) => { b: Int => { b.toString + a.toString} }

    val concatFiveCurried = concatIntsFunction(5)
    println(concatFiveCurried(number)) // number + "5"
    val concatIntsUncurried = uncurry(concatIntsFunction)
    println(concatIntsUncurried(5, number)) // number + "5"

    val reCurryConcatInt = curry(concatIntsUncurried)
    val reCurryConcatFive = reCurryConcatInt(5)
    println(reCurryConcatFive(number)) // number + "5"

    val addTwo = (b: Int) => { b + 2 }
    val addThree = (a: Int) => {a + 3}
    val composed = compose(addTwo, addThree)
    println(composed(number)) // number + 5

    val addFive = addTwo andThen addThree
    println(addFive(number)) // number + 5
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = { a: A => f(g(a)) }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) =>  (b: B) => f(a, b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def formatResult(name: String, number: Int, mathFunction: Int => Int): String = {
    val msg = s"The $name of $number is ${mathFunction(number)}"
    msg
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = { if (n <= 0) acc else go(n - 1, n*acc) }
    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(lastTwo: (Int, Int), current: Int): Int = {
      if (current >= n) lastTwo._1
      else {
        val nextTwo: (Int, Int) = (lastTwo._2, lastTwo._1 + lastTwo._2)
        go(nextTwo, current + 1)
      }
    }
    go((0,1), 1)
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

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int, lastA: A): Boolean = {
      if (n >= as.length) true
      else if (!ordered(as(n), lastA)) false
      else loop(n + 1, as(n))
    }
    loop(0, as(0))
  }
}
object Part1Section3 extends App {
  /*
  This is simply a data type declaration
  A trait is an abstract interface with or without methods on it (in our case without)
  This is almost exactly the same as an abstract class:
      abstract class List[+A]
  Sealed implies that all implementations of this trait must exist in this file
  A is our parameterized type, it can be a list of anything basically
  The plus means the list will be covariant with regards to A, so you could declare:
      val list: List[ParentClass] = List[ChildClass]()
  If we left off the + we would only be able to assign a list of ParentClass to type ParentClass
  Wow that's a lot to unpack...
  */
  sealed trait List[+A]

  /*
  A case is a constructor or implementation of List
  Here we are creating a nil object which is a child of our list object with Nothing as the type of data it will hold.
  */
  case object Nil extends List[Nothing]

  /*
  This is a second case constructor, (Cons for short)
  This is a constructor for a list that is not empty. It has a head object an the rest of the list as parameters.
   */
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  // Companion object for our List type, we can attach functions that can be called off of Lists here
  object List {

    // Creates a LL full of `a` using our Cons
    def fill[A](n: Int, a: A): List[A] = {
      @scala.annotation.tailrec
      def go(i: Int, list: List[A]): List[A] = {
        if(i == n) list
        else go(i + 1, Cons(a, list))
      }
      go(1, Cons(a, Nil))
    }

    // Just for fun fills a list with integers from 1 to n
    def fillWithSequence(n: Int): List[Int] = {
      @scala.annotation.tailrec
      def go(i: Int, list: List[Int]): List[Int] = {
        if(i < 2) list
        else go(i - 1, Cons(i - 1, list))
      }
      go(n, Cons(n, Nil))
    }

    // Just for fun, create a function that calls another function on each object in the list
    @scala.annotation.tailrec
    def iterate[A, B](as: List[A], f: A => B): Unit = as match {
      case Nil =>
      case Cons(x, xs) =>
        f(x)
        iterate(xs, f)
    }

    // Multiplies each element
    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }

    // Takes N arguments and recursively adds them to a list
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  val is: List[Int] = List.fillWithSequence(5)
  List.iterate(is, println)

}
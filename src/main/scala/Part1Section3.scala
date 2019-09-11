import Part1Section3.x
import Part1Section3Notes.List.sum
import Part1Section3Notes._

object Part1Section3 extends App {

  // Exercise 3.1
  val x: Int = List(1,2,3,4,5) match {
    // List(anything, 2, 4, anything)
    case Cons(x, Cons(2, Cons(4, _))) => x
    // Nil
    case Nil => 42
    // List(anything, anything, 3, 4, anything) *** this get's called for (1, 2, 3, 4, 5)
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) =>x+y
    // List(anything, anything)
    // This would get called if it was above the previous case for list 1,2,3,4,5
    case Cons(h, t) => h + sum(t)
    // Anything else, can't figure out how to get this to be honest...
    case _ => 101
  }

  // Exercise 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(_, xs) => xs
  }

  // Exercise 3.3
  def setHead[A](as: List[A], newHead: A): List[A] = as match {
    case Nil => Cons(newHead, Nil)
    case Cons(_, xs) => Cons(newHead, xs)
  }

  // Exercise 3.4
  def drop[A](as: List[A], n: Int): List[A] = {
    @scala.annotation.tailrec
    def go(list: List[A], i: Int): List[A] = list match {
      case Nil => Nil
      case Cons(_, Nil) => if(i == n) list else Nil
      case Cons(_, xs) => if(i < n) go(xs, i + 1) else list
    }
    go(as, 0)
  }

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @scala.annotation.tailrec
    def go(list: List[A]): List[A] = list match {
      case Nil => Nil
      case Cons(x, xs) => if(!f(x)) list else go(xs)
    }
    go(l)
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  val is = List(1,2,3,4,5)

  println("original")
  List.iterate(is, println)

  println("\ntail removed")
  List.iterate(tail(is), println)

  println("\nsetting 0 to head")
  List.iterate(setHead(is, 0), println)

  println("\nremoving first 2 items")
  List.iterate(drop(is, 2), println)

  println("\nremoving till we don't get a 1 2 or 3")
  val odd: Int => Boolean = (i: Int) => i == 1 || i == 2 || i == 3
  List.iterate(dropWhile(is, odd), println)

  println("\nremove last element")
  List.iterate(init(is), println)
}

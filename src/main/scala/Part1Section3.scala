import Part1Section3Notes.Nil
import Part1Section3Notes.List.sum
import Part1Section3Notes._

object Part1Section3 extends App {

  // Exercise 3.1
  val y: Int = List(1,2,3,4,5) match {
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

  // Exercise 3.9
  def length[A](as: List[A]): Int = {
    val f = (_: A, n: Int) => n + 1
    foldRight(as, 0)(f)
  }

  // Exercise 3.10
  def foldLeft[A,B](as: List[A], z: B)(g: (B, A) => B): B = {
    @scala.annotation.tailrec
    def go(n: B, list: List[A]): B = list match {
      case Nil => n
      case Cons(x, xs) => go(g(n,x),xs)
    }
    go(z, as)
  }

  // Exercise 3.11
  def sumFoldLeft(is: List[Int]): Int = foldLeft(is, 0)(_ + _)
  def productFoldLeft(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  // Exercise 3.12
  def reverseList[A](as: List[A]): List[A] = {
    foldLeft(as, List[A]())((prev: List[A], next: A) => Cons(next, prev))
  }

  // Exercise 3.14
  def append[A](firstList: List[A], secondList: List[A]): List[A] = {
    foldRight(firstList, secondList)((h,t) => Cons(h, t))
  }

  // Exercise 3.15
  def concatListofLists[A](ls: List[List[A]]): List[A] = {
    foldRight(ls, List[A]())(append)
  }

  // Exercise 3.16
  def addOne(as: List[Int]): List[Int] = {
    foldRight(as, List[Int]())((h, t) => Cons(h + 1, t))
  }

  // Exercise 3.17
  def doubleToString(as: List[Double]): List[String] = {
    foldRight(as, List[String]())((h, t) => Cons(h.toString, t))
  }

  // Exercise 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, List[B]())((h,t) => Cons(f(h), t))
  }

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, List[A]())((h,t) => if(f(h)) Cons(h, t)  else t)
  }

  // Exercise 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, List[B]())((h: A, t: List[B]) => concatListofLists(List(f(h), t)))
  }

  // Exercise 3.21
  def flatMapFilter[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)((a: A) => if(f(a)) Nil else List(a))
  }

  // Exercise 3.22
  def addLists(isOne: List[Int], isTwo: List[Int]): List[Int] = {
    def go(x: List[Int], z: List[Int], newList: List[Int]): List[Int] = (x,z) match  {
      case (Cons(xh, xt), Cons(zh, zt)) => Cons(xh + zh, go(xt, zt, newList))
      case (Nil, Cons(zh, zt)) => Cons(zh, go(Nil, zt, newList))
      case (Cons(xh, xt), Nil) => Cons(xh, go(xt, Nil, newList))
      case (Nil, Nil) => newList
    }
    go(isOne, isTwo, List[Int]())
  }

  val is = List(3, 9, 1, 11)
  val is2 = List(3, 9, 1, 11, 300)
  println(addLists(is,is2))

  println(flatMapFilter(is)(d => d % 3 == 0))
  //val ss: List[String] = doubleToString(is)

  println(append(is, List(12.0, 3.0)))

  //println(productFoldLeft(is))
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
  //List.iterate(dropWhile(is, odd), println)

  println("\nremove last element")
  List.iterate(init(is), println)
}

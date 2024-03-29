import Part1Section3ListNotes.List.sum
import Part1Section3ListNotes._
import Part1Section3TreeNotes.{Branch, Leaf, Tree}

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
    case Nil => Nil
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
  // My implementation is a bit fancier than is maybe needed, if one list is larger than the other I push the extra
  // items onto the resulting list after adding up subsets of the larger with the smaller.
  def addLists(isOne: List[Int], isTwo: List[Int]): List[Int] = {
    def go(x: List[Int], z: List[Int], newList: List[Int]): List[Int] = (x,z) match  {
      case (Cons(xh, xt), Cons(zh, zt)) => Cons(xh + zh, go(xt, zt, newList))
      case (Nil, Cons(zh, zt)) => Cons(zh, go(Nil, zt, newList))
      case (Cons(xh, xt), Nil) => Cons(xh, go(xt, Nil, newList))
      case (Nil, Nil) => newList
    }
    go(isOne, isTwo, List[Int]())
  }

  // Exercise 3.23
  def zipWith[A, B](asOne: List[A], asTwo: List[A])(f: (A, A) => B): List[B] = {
    def go(x: List[A], z: List[A], newList: List[B]): List[B] = (x,z) match  {
      case (Cons(xh, xt), Cons(zh, zt)) => Cons(f(xh, zh), go(xt, zt, newList))
      case (_, _) => newList
    }
    go(asOne, asTwo, List[B]())
  }

  // Exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @scala.annotation.tailrec
    def search(aSup: List[A], aSub: List[A], seeking: Boolean): Boolean = (aSup, aSub) match {
      case (Cons(supHead, supTail), Cons(subHead, subTail)) =>
        if(subHead == supHead)
          // We found a match, start seeking
          search(supTail, subTail, seeking = true)
        else
          // If we were seeking our chain has been broken, start over with the original sub, but continue on with tail
          search(supTail, sub, seeking=false)
      // If the first list is finished but we still have items in the sub list, return false
      case (Nil, Cons(_, _)) => false
      // If we reached this default case return seeking
      case (_, _) => seeking
    }
    search(sup, sub, seeking = false)
  }

  // Exercise 3.25
  def treeSize[A](tree: Tree[A]): Int = tree match {
    case Branch(left, right) => 1 + treeSize(left) + treeSize(right)
    case Leaf(_) => 1
  }

  // Exercise 3.26
  def treeMax(tree: Tree[Int]): Int = {
    def findMax(search: Tree[Int], currentMax: Int): Int = search match {
      case Branch(left, right) => findMax(left, currentMax) max findMax(right, currentMax)
      case Leaf(n) => currentMax max n
    }
    findMax(tree, currentMax = -1)
  }

  // Exercise 3.27
  def maxDepth[A](tree: Tree[A]): Int =  {
    def findDepth(search: Tree[A], currentDepth: Int, maxDepth: Int): Int = search match {
      case Branch(left, right) => findDepth(left, currentDepth + 1, maxDepth) max
        findDepth(right, currentDepth + 1, maxDepth)
      case Leaf(_) => currentDepth max maxDepth
    }
    findDepth(tree, 0, 0)
  }

  // Exercise 3.28
  def treeMap[A](tree: Tree[A])(f: (A => A)): Tree[A] = {
    def map(tree: Tree[A]): Tree[A] = tree match {
      case Branch(left, right) => Branch(map(left), map(right))
      case Leaf(n) => Leaf(f(n))
    }
    map(tree)
  }

  // Exercise 3.29
  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
    case Leaf(n) => l(n)
    case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
  }

  def sizeFold[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)(_ + _ + 1)
  }

  def maxFold(t: Tree[Int]): Int = {
    fold(t)(n => n)(_ max _)
  }

  val tree = Branch(Branch(Branch(Leaf(10000), Leaf(1)), Leaf(10)), Branch(Leaf(20000), Branch(Branch(Leaf(1), Leaf(1)), Leaf(1))))

  println(maxFold(tree))
}

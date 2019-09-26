object Part1Section3TreeNotes {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def apply[A](as: A*): List[A] =
      Cons(as.head, apply(as.tail: _*))
  }
}

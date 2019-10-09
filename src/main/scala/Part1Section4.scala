

object Part1Section4 extends App {
  // Exercise 4.1
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def flatMapMatch[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(a) => f(a)
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
      this map f getOrElse None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(x) => x
    }

    def orElseIf[B >: A](ob: => Option[B]): Option[B] = {
      if(this == None) ob
      else this
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] =
      this.map(Some(_)).getOrElse(ob)

    def filterMatch(f: A => Boolean): Option[A] = this match {
      case Some(a) => if(f(a)) this else None
      case None => None
    }

    def filter(f: A => Boolean): Option[A] = {
      flatMap(a => if (f(a)) Some(a) else None)
    }

  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  // Exercise 4.2
  def mean(xs: Seq[Double]): Double = {
    xs.foldLeft(0.0)(_ + _) / xs.length
  }
  def variance(xs: Seq[Double]): Option[Double] = {
    // This is hideous and I hate it
    Some(mean(xs)).flatMap(m => Some(mean(xs.map(x => math.pow(x - m, 2)))))
  }

  // Exercise 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =  {
    for {
      aa <- a
      bb <- b
    } yield f(aa,bb)
  }

  // Exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => if(h.getOrElse(None) == None) None else h.flatMap(hh => sequence(t).map(hh :: _))
  }

  // Exercise 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  // Exercise 4.6
  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      for {
        a <- this
        bb <- b
      } yield f(a, bb)
    }

  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  println(Right(1).map(i => i + 1))

}

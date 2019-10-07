object Part1Section4 extends App {
  // Exercise 4.1

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(a) => f(a)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(x) => x
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
      if(this == None) ob
      else this
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(a) => if(f(a)) this else None
      case None => None
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
    Some(mean(xs)) flatMap (m => Some(mean(xs.map(x => math.pow(x - m, 2)))))
  }

}

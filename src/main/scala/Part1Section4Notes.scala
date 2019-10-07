import Part1Section4.{None, Some, Option}

object Part1Section4Notes extends App {
  val emptyList: List[Any] = List()
  // Returns None
  println(emptyList.headOption)
  // Throws Exception
  try { println(emptyList.head) }
  catch { case _: java.util.NoSuchElementException => println("Empty List") }

  case class Employee(name: String, department: String)
  def lookupByName(name: String): Option[Employee] = name.toLowerCase match {
    case "joe" => Some(Employee(name="Joe", department="Science!"))
    case _ => None
  }
  val department: String = lookupByName("Tom").map(_.department).getOrElse("Not Employed")
  println(department) // Not employed

  // a: => A means that we don't evaluate a outside of the function it gets evaluated in the function
  // so Try(1/0) will result in None, if just used a: A then 1/0 would get evaluated before Try is called and would
  // result in an exception.
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {case _: Exception => None}

  def TryFunc[A, B](f: A => B)(a: A): Option[B] = {
    try Some(f(a))
    catch {case _: Exception => None}
  }

  val oops = Try(1 / 0)
  println(oops) // None



}

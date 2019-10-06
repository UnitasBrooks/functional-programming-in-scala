object Part1Section4Notes extends App {
  val emptyList: List[Any] = List()
  // Returns None
  println(emptyList.headOption.getOrElse())
  // Throws Exception
  try { println(emptyList.head) }
  catch { case _: java.util.NoSuchElementException => println("Empty List") }

}

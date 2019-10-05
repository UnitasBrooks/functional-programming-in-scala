import org.scalatest.FunSpec
import Part1Section3._
import Part1Section3ListNotes._

class Part1Section3Test extends FunSpec {
  trait Fixture  {
    val intList = List(1, 2, 3)
    val doubleList = List(1.0, 2.0, 3.0)
  }

  describe("Exercise 3.2 Tail") {
    new Fixture {
      it("should return everything but the head") {
        assert(tail(intList) == List(2,3))
      }

      it("should return Nil when Nil is supplied") {
        assert(tail(Nil) == Nil)
      }
    }
  }

  describe("Exercise 3.3 setHead") {
    new Fixture {
      it("should return a list with new head") {
        assert(setHead(intList, newHead=5) == List(5,2,3))
      }

      it("should create a new list with the new head when Nil is provided") {
        assert(setHead(Nil, newHead=5) == List(5))
      }
    }
  }

  describe("Exercise 3.4 drop") {
    new Fixture {
      it("should remove the first N elements leaving the rest of the list") {
        assert(drop(intList, n=2) == List(3))
      }

      it("should return Nil if the list is Nil") {
        assert(drop(Nil, n=1000) == Nil)
      }

      it("should return Nil if Nil is longer than the list") {
        assert(drop(intList, n=1000) == Nil)
      }
    }
  }

  describe("Exercise 3.5 dropWhile") {
    new Fixture {
      it("should remove items from the list while a condition is met") {
        assert(dropWhile(intList, (i: Int) => i == 1 || i == 2) == List(3))
      }
    }
  }

  describe("Exercise 3.6 Init") {
    new Fixture {
      it("should remove the last element from the list") {
        assert(init(intList) == List(1, 2))
      }

      it("should return Nil when a one item list is supplied") {
        assert(init(List(1)) == Nil)
      }

      it("should return Nil when Nil is supplied") {
        assert(init(Nil) == Nil)
      }
    }
  }

  describe("Exercise 3.9 length") {
    new Fixture {
      it("should return the length of the list") {
        assert(length(intList) == 3)
      }

      it("should return 0 when Nil or a empty list is supplied") {
        assert(length(Nil) == 0)
        assert(length(List()) == 0)
      }
    }
  }

  describe("Exercise 3.10 + 3.11") {
    new Fixture {
      it("should sum all items in the list") {
        assert(sumFoldLeft(intList) == 6)
      }

      it("should return the product of all items in the list") {
        assert(productFoldLeft(doubleList) == 6)
      }
    }
  }

  describe("Exercise 3.12 reverse") {
    new Fixture {
      it("should return the list supplied in reverse") {
        assert(reverseList(intList) == List(3, 2, 1))
      }
    }
  }

}
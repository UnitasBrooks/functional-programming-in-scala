import Part1Section4.{None, Some, Option, variance, map2, sequence}
import org.scalatest.FunSpec


class Part1Section4Test extends FunSpec {
  val addOne: (Int => Int) = (i: Int) => i + 1
  val threeDividedBy: (Int => Option[Int]) = (i: Int) => if(i == 0) None else Some(3 / i)
  val isThree: (Int => Boolean) = (i: Int) => i == 3
  val add: ((Int, Int) => Int) = (i: Int, j: Int) => i + j

  describe("Excercise 4.1 map") {
    it("should return the result of the function when something is provided") {
      assert(Some(3).map(addOne) == Some(4))
    }
    it("should return None when called on None") {
      assert(None.map(addOne) == None)
    }
  }

  describe("4.1 flatMap") {
    it("should return the result of the function when Some(_) is provided") {
      assert(Some(1).flatMap(threeDividedBy) == Some(3))
    }
    it("should return None when an error state is met") {
      assert(Some(0).flatMap(threeDividedBy) == None)
    }
    it("should return None when called on None") {
      assert(None.flatMap(threeDividedBy) == None)
    }
  }

  describe("Exercise 4.1 getOrElse") {
    it("should return the value if it is not None") {
      assert(Some(1).getOrElse(10) == 1)
    }
    it("should return the default if it is None") {
      assert(None.getOrElse(10) == 10)
    }
  }

  describe("Exercise 4.1 orElse") {
    it("should return the called option when something is supplied") {
      assert(Some(1).orElse(Some(2)) == Some(1))
    }
    it("should return the default Option if called on None") {
      assert(None.orElse(Some(2)) == Some(2))
    }
  }

  describe("Exercise 4.1 filter") {
    it("should return itself if the condition is met") {
      assert(Some(3).filter(isThree) == Some(3))
    }
    it("should return None if the condition is not met") {
      assert(Some(2).filter(isThree) == None)
    }
    it("should return None if called on None") {
      assert(None.filter(isThree) == None)
    }
  }

  describe("Exercise 4.2 variance") {
    it("should calculate the variance of a given sequence") {
      assert(variance(Seq(1.0, 2.0, 3.0)) == Some(0.6666666666666666))
    }
  }

  describe("Exercise 4.3 map2") {
    it("should take a non-option function and and allow it to work the option") {
      assert(map2(Some(1), Some(2))(add) == Some(3))
    }

    it("should return None if either parameters are None") {
      assert(map2(None, Some(2))(add) == None)
    }
  }

  describe("Exercise 4.4 sequence") {
    it("should return an Option containing a list") {
      assert(sequence(List(Some(1),Some(2),Some(3))) == Some(List(1,2,3)))
    }
    it("should return None if None is in the list") {
      assert(sequence(List(Some(1),None,Some(3))) == None)
    }
  }


}

import Part1Section2._
import org.scalatest.FunSpec



class Part1Section2Test extends FunSpec {
  trait Fixture  {
    val divideThree: Int => Int = (i: Int) => i / 3
    val timesTen: Int => Int = (n: Int) => n * 10
    val intGreaterThan: (Int, Int) => Boolean = (i: Int, j: Int) => { i >= j}
    val concatInts: (Int, Int) => String = (x: Int, y: Int) => (x.toString + y.toString)
    val sortedArray: Array[Int] = Array(1, 2, 3, 4, 5, 6)
    val array: Array[Int] = Array(1, 5, 3, 4, 4, 0)
  }

  describe("compose") {
    new Fixture {
      it("should call function f then function g with the result of f") {
        val composed = compose(divideThree, timesTen)
        assert(composed(9) == 30)
      }
    }
  }

  describe("curry") {
    new Fixture {
      it("should set parameter one and return a function that takes a single parameter") {
        val curried = curry(concatInts)
        val prependSix = curried(6)
        assert(prependSix(0) == "60")
      }
    }
  }

  describe("uncurry") {
    new Fixture {
      it("should take two single parameter functions and return a two parameter function") {
        val uncurried = uncurry(curry(concatInts))
        assert(uncurried(1,2) == "12")
      }
    }
  }

  describe("isSorted") {
    new Fixture {
      it("should find that the array is sorted when we provide a presorted array of integers") {
        assert(isSorted(sortedArray, intGreaterThan))
      }

      it("should find that the arrays is not sorted when provide a unsorted array") {
        assert(!isSorted(array, intGreaterThan))
      }
    }
  }

  describe("formatResult") {
    new Fixture {
      it("should create a string based on the result of a (Int, Int) function") {
        assert(formatResult("times ten", 3, timesTen) == "The times ten of 3 is 30")
      }
    }
  }

  describe("fib") {
    it("should return 1 when 1 is supplied") {
      assert(fib(1) == 1)
    }

    it("should return 55 when 10 is supplied") {
      assert(fib(10) == 55)
    }

  }





}


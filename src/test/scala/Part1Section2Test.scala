import Part1Section2._
import org.scalatest.FunSpec



class Part1Section2Test extends FunSpec {
  trait Fixture  {
    val divideThree: Double => Double = (i: Double) => i / 3
    val timesTen: Double => Double = (n: Double) => n * 10
    val concatInts: (Int, Int) => String = (x: Int, y: Int) => (x.toString + y.toString)
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
      it("it should take two single parameter functions and return a two parameter function") {
        val uncurried = uncurry(curry(concatInts))
        assert(uncurried(1,2) == "12")
      }
    }
  }




}


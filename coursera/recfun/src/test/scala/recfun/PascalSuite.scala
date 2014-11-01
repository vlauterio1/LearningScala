package recfun

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FlatSpec with Matchers {
  import recfun.Main.pascal

  "A value of 1" should "result for col=0, row=2" in {
    pascal(0,2) should be (2)
  }

  "A value of 2" should "result for col=1, row=2" in {
    pascal(1,2) should be (2)
  }

  "A value of 3" should "result for col=1,row=3" in {
    pascal(1,3) should be (3)
  }

  "A value of 1" should "result for col=0, row=0" in {
    pascal(0, 0) should be (1)
  }

  "A value of 1" should "result for col=2, row=2" in {
    pascal(2, 2) should be (1)
  }

  it should "throw Exception if a negative col is used" in {
    a [Exception] should be thrownBy {
      pascal(-1, 0)
    }
  }

  it should "throw Exception if a negative row is used" in {
    a [Exception] should be thrownBy {
      pascal(0, -2)
    }
  }

  it should "throw Exception if a positive invalid col is used" in {
    a [Exception] should be thrownBy {
      pascal(4, 2)
    }
  }
}

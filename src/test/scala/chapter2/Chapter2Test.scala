package chapter2

import org.scalatest.FunSuite

class Chapter2Test extends FunSuite {
  test("chapter2.Chapter2.curry") {
    val curried = Chapter2.curry[Int,Double,Boolean]((i: Int, d: Double) => i < d: Boolean)
    assert(curried(1)(2.0))
    assert(!curried(2)(1.0))
  }
}

package chapter2

import org.scalatest.FunSuite

class Chapter2Test extends FunSuite {
  test("chapter2.Chapter2.curry") {
    val curried = Chapter2.curry[Int,Double,Boolean]((i: Int, d: Double) => i < d: Boolean)
    val curriedWith1 = curried(1)
    assert(curriedWith1(2.0))
    assert(!curriedWith1(0.5))
  }
  test("chapter2.Chapter2.uncurry") {
    val curried = Chapter2.curry[Int,Double,Boolean]((i: Int, d: Double) => i < d: Boolean)
    val uncurried = Chapter2.uncurry[Int,Double,Boolean](curried)
    assert(uncurried(1, 2.0))
    assert(!uncurried(1, 0.5))
  }
  test("chapter2.Chapter2.compose") {
    def g(i: Int): Double =
      i / 2.0
    def f(d: Double): Boolean =
      d * 2 < 10
    val h = Chapter2.compose(f, g)
    assert(h(8))
    assert(!h(11))
  }
}

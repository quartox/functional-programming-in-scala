import java.security.KeyStore.TrustedCertificateEntry

object chapter2 {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(i: Int, acc0: Int, acc1: Int): Int =
      if (i == 0) acc0
      else go(i - 1, acc1, acc0 + acc1)
    go(n, 0, 1)
  }
  private def testFib(): Unit = {
    println(fib(1))
    println(fib(2))
    println(fib(3))
    println(fib(4))
    println(fib(5))
    println(fib(6))
    println(fib(7))
  }
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean = {
      if (n >= as.length-1) true
      else if (!ordered(as(n), as(n+1))) false
      else go(n+1)
    }
    go(0)
  }
  private def testSorted(): Unit = {
    def orderedImplicitOrdering[A](a: A, b: A)(implicit ordering:Ordering[A]): Boolean =
      ordering.lteq(a, b)
    def orderedContextBound[A : Ordering](a: A, b: A): Boolean = {
      val ordering = implicitly[Ordering[A]]
      ordering.lteq(a, b)
    }
    def orderedViewBound[A <% Ordered[A]](a: A, b: A): Boolean =
      a <= b
    def orderedImplicitOrdered[A](a: A, b: A)(implicit ev$1: A => Ordered[A]): Boolean =
      a <= b

//    def ordered[A <: Ordered[A]](a: A, b: A): Boolean =
//      a <= b
//    val aIntOrdered = Array(1, 2, 3)
//    val sortedOrderedTest = isSorted(aIntOrdered, ordered[Int])
//    assert(sortedOrderedTest)
//    def ordered[Ordered[A]](a: Ordered[A], b: Ordered[A]): Boolean =
//      a <= b

    val aInt = Array(1, 2, 3)
    val sortedTest = isSorted(aInt, orderedImplicitOrdering[Int])
    assert(sortedTest)
    val aStr = Array("a", "c", "b")
    val unSortedTest = isSorted(aStr, orderedContextBound[String])
    assert(!unSortedTest)
    val aIntEqual = Array(1, 1)
    val equalTest = isSorted(aIntEqual, orderedViewBound[Int])
    assert(equalTest)
    val aDbl = Array(1.0d, 0.99d)
    val doubleUnsortedTest = isSorted(aDbl, orderedImplicitOrdered[Double])
    assert(!doubleUnsortedTest)

    val aInt2 = Array(1, 2, 3)
    val sortedTest2 = isSorted(aInt2, (a: Int, b: Int)=> a <= b)
    assert(sortedTest2)
  }
  def main(args: Array[String]): Unit = {
    //testFib()
    testSorted()
  }
}
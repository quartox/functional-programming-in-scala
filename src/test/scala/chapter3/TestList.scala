package chapter3

import org.scalatest.flatspec.AnyFlatSpec

class TestList extends AnyFlatSpec {
  "tail" should "return a list without the head" in {
    assert(List.tail(List(1,2,3)) === List(2,3))
  }
  it should "return nil when tail is nil" in {
    assert(List.tail(List(1)) === Nil)
    assert(List.tail(Nil) === Nil)
  }

  "setHead" should "return a list with a new head" in {
    assert(List.setHead(List(1,2,3), 4) === List(4,2,3))
  }
  it should "return nil when list is empty" in {
    assert(List.tail(Nil) === Nil)
  }
  it should "set the single element if there is only one element" in {
    assert(List.setHead(List(1), 4) === List(4))
  }

  "drop" should "return the remaining elements of a list" in {
    assert(List.drop(List(1,2,3), 2) === List(3))
  }
  it should "return an empty list if it reads off the end of a list" in {
    assert(List.drop(List(1,2,3),4) === Nil)
  }
  it should "return an empty list if reading from an empty list" in {
    assert(List.drop(Nil, 1) === Nil)
  }
  it should "return the full list when dropping 0 elements" in {
    assert(List.drop(List(1,2,3), 0) === List(1,2,3))
  }

  "dropWhile" should "drop all elements matching a predicate" in {
    val l = List(-2,-1,0,-1)
    def f(i: Int): Boolean = i < 0
    assert(List.dropWhile(l, f) === List(0,-1))
  }
  it should "return an empty list of all elements match predicate" in {
    val l = List(-2,-1)
    def f(i: Int): Boolean = i < 0
    assert(List.dropWhile(l, f) === Nil)
  }
  it should "return an empty list if given an empty list" in {
    assert(List.dropWhile(Nil, (i: Int) => i < 0) === Nil)
  }

  "init" should "return all but last element of list" in {
    assert(List.init(List(1,2,3,4)) === List(1,2,3))
  }
  it should "return an empty list if given a one element list" in {
    assert(List.init(List(1)) === Nil)
  }
  it should "return an empty list if given an empty list" in {
    assert(List.init(Nil) === Nil)
  }

  "foldRight" should "construct a list when passed Nil and Cons" in {
    assert(List.foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_)) === List(1,2,3))
  }

  "length" should "return the length of the list" in {
    assert(List.length(List(1,2,3)) === 3)
    assert(List.length(List(1)) === 1)
  }
  it should "return 0 for an empty list" in {
    assert(List.length(List()) === 0)
  }
}

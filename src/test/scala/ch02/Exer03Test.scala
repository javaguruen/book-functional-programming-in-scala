package ch02

import org.scalatest.FunSuite

class Exer03Test extends FunSuite {
  val exer03 = new Exer03

  test("Currying"){
    def times(a: Int, b: Int) = a * b
    def curriedTimes = exer03.curry(times)
    def times3 = curriedTimes(3)
    val value = times3(7)
    assert(value === 21)

    assert( 21 === curriedTimes(3)(7))
  }
}

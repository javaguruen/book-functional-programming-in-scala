package ch02

import org.scalatest.FunSuite

class Exer01Test extends FunSuite {
  val exer01 = new Exer01

  test("fibonacci"){
    assert( exer01.fibonacci(1) == 1)
    assert( exer01.fibonacci(2) == 1)
    assert( exer01.fibonacci(3) == 2)
    assert( exer01.fibonacci(4) == 3)
    assert( exer01.fibonacci(5) == 5)
  }
}

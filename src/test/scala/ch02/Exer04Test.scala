package ch02

import org.scalatest.FunSuite

class Exer04Test extends FunSuite {
  val exer = new Exer04

  test("Uncurry"){
    def sum(a: Int) = (b: Int) => a + b

    def uncurriedSum = exer.uncurry(sum)
    val value = uncurriedSum(3, 4)
    assert( 7 === value)
  }
}

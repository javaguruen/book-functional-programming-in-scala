package ch02

import org.scalatest.FunSuite

class Exer05Test extends FunSuite {
  val exer05 = new Exer05

  test("Compose"){
    def double(n: Int) = 2 * n
    def square(n: Int) = n * n

    def composed = exer05.compose(double, square)

    assert( composed(3) === 18)
  }
}

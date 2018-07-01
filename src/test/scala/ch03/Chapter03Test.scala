package ch03

import org.scalatest.FunSuite

class Chapter03Test extends FunSuite {

  test("init returns a list without last element"){
    val original = List(1, 2, 3, 4)
    val modified = List.init(original)
    assert( modified == List(1, 2, 3))
  }
}

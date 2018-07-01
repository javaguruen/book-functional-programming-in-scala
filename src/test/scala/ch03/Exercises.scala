package ch03

import org.scalatest.FunSuite

class Exercises extends FunSuite {
  val exer01 = new Excer01()

  test("init"){
    val initial = List.init( List(1, 2, 3, 4))
    assert(initial === List(1, 2, 3))
  }

  test("Nil to product"){
    val folded = List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    println(folded)
  }

  test("length"){
    assert( List.length( List(1, 2, 3)) === 3)
  }

  test("foldLeft"){
    assert( List.foldLeft(List(1, 2, 3), 0)(_+_) === 6)
  }

  test("sum by foldLeft"){
    assert( List.sumFL(List(1, 2, 3)) === 6)
  }

  test("product by foldLeft"){
    assert( List.productFL(List(1, 2, 3)) === 6)
  }

  test("length by foldLeft"){
    assert( List.lengthFL(List(1, 2, 3)) === 3)
  }

  test("reverse"){
    assert( List.reverse(List(1, 2, 3, 4)) === List(4, 3, 2, 1))
  }
}

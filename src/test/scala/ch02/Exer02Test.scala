package ch02

import org.scalatest.FunSuite

class Exer02Test extends FunSuite {

  test("Sorted array"){
    assert( new Exer02().isSorted(Array(1,2,3,4), (a:Int,b:Int) => a <=b ))
  }

  test("Insorted array"){
    assert(false === new Exer02().isSorted(Array(1,2,4,3), (a:Int,b:Int) => a <=b ))
  }
}

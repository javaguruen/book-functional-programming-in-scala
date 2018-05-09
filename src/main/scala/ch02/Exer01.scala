package ch02

import scala.annotation.tailrec

class Exer01 {

  def fibonacciNonTail(n: Int): Int = {
    if( n == 1) 1
    else if( n == 2) 1
    else fibonacciNonTail(n-1) + fibonacciNonTail(n-2)
  }

  def fibonacci(n: Int): Int = {
    @tailrec
    def fibo(i: Int, fi1: Int, fi2: Int): Int = {
      if (i >= n) fi1 + fi2
      else fibo(i+1, fi1 + fi2, fi1)
    }

    if( n == 1) 1
    else if( n == 2) 1
    else fibo(3, 1, 1)
  }
}

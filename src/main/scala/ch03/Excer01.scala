package ch03

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) {
      Nil
    }
    else {
      Cons(as.head, apply(as.tail: _*))
    }

  //Exercise 3.1
  def output() = {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    x
  }

  //Exercise 3.2
  def tail[A](list: List[A]): List[A] = {
    list match {
      case Cons(x, tail) => tail
      case Nil => Nil
    }
  }

  //Exercise 3.3
  def setHead[A](newHead: A, xs: List[A]): List[A] = {
    xs match {
      case Nil => Cons(newHead, Nil)
      case Cons(x, tail) => Cons(newHead, tail)
    }
  }

  //Exercise 3.4
  def drop[A](list: List[A], n: Int): List[A] = {
    if (n > 0) {
      drop(tail(list), n - 1)
    }
    else {
      list
    }
  }

  //Exercise 3.5
  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = {
    list match {
      case Nil => Nil
      case Cons(x, xs) =>
        if (f(x)) {
          dropWhile(xs, f)
        }
        else {
          list
        }
    }
  }

  //Exercise 3.5
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => l
      case Cons(c1, Cons(x, Nil)) => Cons(c1, Nil)
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  //Exercise 3.5
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def main(args: Array[String]) = {
    println(output())
  }

  //Exercise 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((elem, len) => len + 1)
  }

  //Exercise 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(as: List[A], z: B): B = {
      as match {
        case Nil => z
        case Cons(x, xs) => loop(xs, f(z, x))
      }
    }

    loop(as, z)
  }

  def lengthFL[A](as: List[A]): Int = {
    foldLeft(as, 0)((len, elem) => len + 1)
  }

  def sumFL(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def productFL(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  //Exercise 3.12
  def reverse[A](ns: List[A]): List[A] = {
    foldLeft(ns, List[A]())((acc, elem) => Cons(elem, acc))
  }

  //Exercise 3.13
  def foldRightByFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }


}

class Excer01 {
}

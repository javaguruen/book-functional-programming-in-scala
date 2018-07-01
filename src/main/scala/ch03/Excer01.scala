package ch03

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
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

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
    if( n > 0) drop(tail(list), n-1)
    else list
  }

  //Exercise 3.5
  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = {
    list match {
      case Nil => Nil
      case Cons(x, xs) =>
        if( f(x) ) dropWhile(xs, f)
        else list
    }
  }

  //Exercise 3.6
  def init[A](l: List[A]): List[A] = {
    def loop[A](xs: List[A], agg: List[A]): List[A] = {
      xs match {
        case Cons(x, Nil) =>
          agg
        case Cons(head, tail) =>
          loop(tail, Cons(head, agg))
      }
    }
    loop(l, Nil)
  }

  def  main(args: Array[String]) = {
    println( output() )
  }
}

class Excer01 {

}

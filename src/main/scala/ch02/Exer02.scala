package ch02

class Exer02 {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(i: Int): Boolean = {
      if (i >= as.length) true
      else if (ordered(as(i - 1), as(i))) loop(i + 1)
      else false
    }

    if (as.length < 2) true
    else loop(1


























































































































    )
  }
}

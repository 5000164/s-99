package jp._5000164.s_99

/**
  * http://aperiodic.net/phil/scala/s-99/
  */
object WorkingWithLists extends App {
  // Find the last element of a list.
  assert(last(List(1, 1, 2, 3, 5, 8)) == 8)
  assert(last(List("a", "b", "c")) == "c")
  assert(lastAlt(List(1, 1, 2, 3, 5, 8)) == 8)
  assert(lastAlt(List("a", "b", "c")) == "c")

  def last[A](list: List[A]): A = list.last

  def lastAlt[A](list: List[A]): A = list match {
    case h :: Nil => h
    case _ :: tail => lastAlt(tail)
    case _ => throw new NoSuchElementException
  }

  // Find the last but one element of a list.
  assert(penultimate(List(1, 1, 2, 3, 5, 8)) == 5)
  assert(penultimate(List("a", "b", "c")) == "b")
  assert(penultimateAlt(List(1, 1, 2, 3, 5, 8)) == 5)
  assert(penultimateAlt(List("a", "b", "c")) == "b")
  assert(lastNth(2, List(1, 1, 2, 3, 5, 8)) == 5)
  assert(lastNth(2, List("a", "b", "c")) == "b")
  assert(lastNthAlt(2, List(1, 1, 2, 3, 5, 8)) == 5)
  assert(lastNthAlt(2, List("a", "b", "c")) == "b")

  def penultimate[A](list: List[A]): A =
    if (list.isEmpty) throw new NoSuchElementException
    else list.init.last

  def penultimateAlt[A](list: List[A]): A = list match {
    case h :: _ :: Nil => h
    case _ :: tail => penultimateAlt(tail)
    case _ => throw new NoSuchElementException
  }

  def lastNth[A](n: Int, list: List[A]): A = {
    if (n <= 0) throw new IllegalArgumentException
    if (list.lengthCompare(n) < 0) throw new NoSuchElementException
    list.takeRight(n).head
  }

  def lastNthAlt[A](n: Int, list: List[A]): A = {
    def lastNthR(count: Int, resultList: List[A], curList: List[A]): A = curList match {
      case Nil if count > 0 => throw new NoSuchElementException
      case Nil => resultList.head
      case _ :: tail => lastNthR(count - 1, if (count > 0) resultList else resultList.tail, tail)
    }

    if (n <= 0) throw new IllegalArgumentException
    else lastNthR(n, list, list)
  }

  // Find the Kth element of a list.
  assert(nth(2, List(1, 1, 2, 3, 5, 8)) == 2)
  assert(nth(2, List("a", "b", "c")) == "c")
  assert(nthAlt(2, List(1, 1, 2, 3, 5, 8)) == 2)
  assert(nthAlt(2, List("a", "b", "c")) == "c")

  def nth[A](n: Int, list: List[A]): A =
    if (n >= 0) list(n)
    else throw new NoSuchElementException

  def nthAlt[A](n: Int, list: List[A]): A = (n, list) match {
    case (0, h :: _) => h
    case (i, _ :: tail) => nthAlt(i - 1, tail)
    case (_, Nil) => throw new NoSuchElementException
  }

  // Find the number of elements of a list.
  assert(length(List(1, 1, 2, 3, 5, 8)) == 6)
  assert(length(List("a", "b", "c")) == 3)

  def length[A](list: List[A]): Int = list.length
}

package jp._5000164.s_99

/**
  * http://aperiodic.net/phil/scala/s-99/
  */
object WorkingWithLists extends App {
  // P01: Find the last element of a list.
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

  // P02: Find the last but one element of a list.
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

  // P03: Find the Kth element of a list.
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

  // P04: Find the number of elements of a list.
  assert(length(List(1, 1, 2, 3, 5, 8)) == 6)
  assert(length(List("a", "b", "c")) == 3)
  assert(lengthAlt(List(1, 1, 2, 3, 5, 8)) == 6)
  assert(lengthAlt(List("a", "b", "c")) == 3)
  assert(lengthAltTail(List(1, 1, 2, 3, 5, 8)) == 6)
  assert(lengthAltTail(List("a", "b", "c")) == 3)
  assert(lengthAltFunctional(List(1, 1, 2, 3, 5, 8)) == 6)
  assert(lengthAltFunctional(List("a", "b", "c")) == 3)

  def length[A](list: List[A]): Int = list.length

  def lengthAlt[A](list: List[A]): Int = list match {
    case Nil => 0
    case _ :: tail => 1 + lengthAlt(tail)
  }

  def lengthAltTail[A](list: List[A]): Int = {
    def lengthR[B](result: Int, curList: List[B]): Int = curList match {
      case Nil => result
      case _ :: tail => lengthR(result + 1, tail)
    }

    lengthR(0, list)
  }

  def lengthAltFunctional[A](list: List[A]): Int = list.foldLeft(0) { (c, _) => c + 1 }

  // P05: Reverse a list.
  assert(reverse(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1))
  assert(reverse(List("a", "b", "c")) == List("c", "b", "a"))
  assert(reverseAlt(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1))
  assert(reverseAlt(List("a", "b", "c")) == List("c", "b", "a"))
  assert(reverseAltTail(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1))
  assert(reverseAltTail(List("a", "b", "c")) == List("c", "b", "a"))
  assert(reverseAltFunctional(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1))
  assert(reverseAltFunctional(List("a", "b", "c")) == List("c", "b", "a"))

  def reverse[A](list: List[A]): List[A] = list.reverse

  def reverseAlt[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case h :: tail => reverseAlt(tail) ::: List(h)
  }

  def reverseAltTail[A](list: List[A]): List[A] = {
    def reverseR[B](result: List[B], curList: List[B]): List[B] = curList match {
      case Nil => result
      case h :: tail => reverseR(h :: result, tail)
    }

    reverseR(Nil, list)
  }

  def reverseAltFunctional[A](list: List[A]): List[A] = list.foldLeft(List[A]()) { (r, h) => h :: r }

  // P06: Find out whether a list is a palindrome.
  assert(isPalindrome(List(1, 2, 3, 2, 1)))
  assert(isPalindrome(List("a", "b", "a")))
  assert(!isPalindrome(List(1, 2, 3)))

  def isPalindrome[A](list: List[A]): Boolean = list == list.reverse

  // P07: Flatten a nested list structure.
  assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))
  assert(flatten(List(List(List("a"), "b"), List("c"))) == List("a", "b", "c"))

  def flatten(list: List[Any]): List[Any] = list flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }
}

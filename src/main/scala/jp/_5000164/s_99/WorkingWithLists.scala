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

  // Find the last but one element of a list.
  assert(penultimate(List(1, 1, 2, 3, 5, 8)) == 5)
  assert(penultimate(List("a", "b", "c")) == "b")

  def last[A](list: List[A]): A = list.last

  def lastAlt[A](list: List[A]): A = list match {
    case h :: Nil => h
    case _ :: tail => lastAlt(tail)
    case _ => throw new NoSuchElementException
  }

  def penultimate[A](list: List[A]): A = last(list.init)
}

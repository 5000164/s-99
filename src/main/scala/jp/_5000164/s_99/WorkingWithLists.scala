package jp._5000164.s_99

/**
  * http://aperiodic.net/phil/scala/s-99/
  */
object WorkingWithLists extends App {
  // Find the last element of a list.
  assert(last(List(1, 1, 2, 3, 5, 8)) == 8)
  assert(last(List("a", "b", "c")) == "c")

  def last[A](list: List[A]): A = list.last
}

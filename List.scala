import scala.language.postfixOps
package functional.datastructures{

abstract class List[+A] {
  def head: A
  def tail: List[A]
  def isEmpty: Boolean
  def length: Int
  def ::[S >: A](item:S):List[S]=new ListImpl[S](item,this)
  def drop(n: Int): List[A]  =  if (isEmpty) ListNil else if (n <= 0) this else tail.drop(n - 1)
  def map[S](f: A => S): List[S] =if (isEmpty) ListNil else f(head) :: tail.map(f)
}



class ListImpl[A](val head: A, val tail: List[A]) extends List[A] {
  def isEmpty = false
  def length: Int = 1 + tail.length

  override def toString: String =   head + " " + tail 
}

object ListNil extends List {
  def head: Nothing = throw new Exception("head of empty list")
  def tail: List[Nothing] = throw new Exception("tail of empty list")
  def isEmpty = true
  def length = 0
  override def toString =  ""
}
object List {
  def apply[A](items: A*): List[A] = {
    var list: List[A] = ListNil.asInstanceOf[List[A]]
    for (idx <- 0 until items.length reverse)
      list = items(idx) :: list
    list
  }
}
}
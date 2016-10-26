import scala.language.postfixOps
package functional.datastructures{

abstract class List[A] {
  def head: A
  def tail: List[A]
  def isEmpty: Boolean
  def length: Int
  def ::(item:A):List[A]=new ListImpl[A](item,this)	
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
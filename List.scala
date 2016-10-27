import scala.language.postfixOps
package functional.datastructures{

abstract class List[+A] {
  private def exceptionHandler(e: String) = throw new NoSuchElementException(e)
  def head: A
  def tail: List[A]
  def isEmpty: Boolean
  def length: Int = 1 + tail.length
  def ::[S >: A](item:S):List[S]=List.generate(item,this)
  def drop(n: Int): List[A]  =  if (isEmpty) ListNil else if (n <= 0) this else tail.drop(n - 1)
  def map[S](f: A => S): List[S] =if (isEmpty) ListNil else f(head) :: tail.map(f)
  def append[S>:A](item:S):List[S]=if (isEmpty) List.generate(item) else List.generate(head,tail.append(item))
  def apply(n: Int): A = if (isEmpty) exceptionHandler("Index out of bounds.") else if (n < 0) exceptionHandler("Index (< 0) out of bounds.") else if (n == 0) head else tail(n - 1)
  def last():A=if (isEmpty) exceptionHandler("Index out of bound") else this(this.length -1)
  def foldLeft[S](accumulator : S )( f : (S,A) => S ): S={
    this match{
      case ListImpl(head,tail) =>{
        val current = f(accumulator, head)
        tail.foldLeft(current)(f)
      }
      case ListNil => accumulator
      }
  }

}
case class ListImpl[A](val head: A, val tail: List[A]) extends List[A] {
  def isEmpty = false
  override def toString: String =   head + " " + tail 
}

object ListNil extends List {
  def head: Nothing = throw new NoSuchElementException("head of empty list")
  def tail: List[Nothing] = throw new NoSuchElementException("tail of empty list")
  def isEmpty = true
  override def length = 0
  override def toString =  ""
}
object List {
  def generate[A](x: A, t: List[A] = ListNil): List[A] =ListImpl(x, t)

  def apply[A](items: A*): List[A] = {
    var list: List[A] = ListNil.asInstanceOf[List[A]]
    for (idx <- 0 until items.length reverse)
      list = items(idx) :: list
    list
  }
}
}

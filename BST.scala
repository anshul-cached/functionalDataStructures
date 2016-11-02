package functional.datastructures{
abstract sealed class Tree[+A<%Ordered[A]]{
	def value:A
	def left:Tree[A]
	def right:Tree[A]
	def isEmpty:Boolean
	def size:Int
	def insert[B >: A <% Ordered[B]](addToTree: B): Tree[B] =
    if (isEmpty) Tree.make(addToTree)
    else if (addToTree < value) Tree.make(value, left.insert(addToTree), right)
    else if (addToTree > value) Tree.make(value, left, right.insert(addToTree))
    else this
 	def throwException(m: String) = throw new NoSuchElementException(m)
}
case object Empty extends Tree[Nothing] {
  def value: Nothing = throwException("Empty Tree Found.")
  def left: Tree[Nothing] = throwException("Empty Tree Found.")
  def right: Tree[Nothing] = throwException("Empty Tree Found.")
  def size: Int = 0

  def isEmpty: Boolean = true
}

case class Branch[A<%Ordered[A]](value:A,left:Tree[A],right:Tree[A],size:Int) extends Tree[A]{
	def isEmpty:Boolean=false

}

object Tree{
	def empty[A]:Tree[A]=Empty
	def make[A <% Ordered[A]](x: A, l: Tree[A] = Empty, r: Tree[A] = Empty): Tree[A] =Branch(x,l,r,l.size+r.size+1)
	def apply[A <% Ordered[A]](arg: A*): Tree[A] = {
    var r: Tree[A] = Tree.empty
    for (x <- arg) r = r.insert(x)
    r
  }

}
}
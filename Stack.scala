package functional.datastructures{

class Stack[+A](self: List[A]){
	
	override def toString()="Stack"
	
	def top: A=self.head

	def rest: Stack[A]=new Stack(self.tail)

	def isEmpty: Boolean=self.isEmpty

	def pop:(A,Stack[A]) =(top,rest)

	def push[B>:A](x:B):Stack[B]=new Stack(x::self)

	def show: List[A]=self
}

object Stack{
	def empty[A]:Stack[A]=new Stack(Nil)
	def apply[A](xs:A*):Stack[A]=xs.foldLeft(Stack.empty[A])((r,x) =>r.push(x))
	}
}

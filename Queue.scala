package functional.datastructures{

case class Queue[+A](in:List[A]=Nil, out:List[A]=Nil) {

def isEmpty: Boolean=(in,out) match{
	case (Nil,Nil) => true
	case (_,_) => false
}
	def enqueue[B>:A](elem:B):Queue[B]=Queue.queueGen(elem::in,out)
	def dequeue: (A, Queue[A]) = out match {
    case head :: tail => (head, new Queue(in, tail))
    case Nil => in.reverse match {
      case head :: tail => (head, new Queue(Nil, tail))
      case Nil => throw new NoSuchElementException("Empty Queue Found")
    }
  }
   def peak: A = dequeue match {
    case (x, _) => x
    }

    def end: Queue[A] = dequeue match{
     case (_, x) => x 
 }

}
object Queue{
	def queueGen[A](in:List[A]=Nil,out:List[A]=Nil)=Queue(in,out)
	def empty[A]:Queue[A]=Queue.queueGen()
	def apply[A](items: A*)= {
		items.foldLeft(Queue.empty[A]){
			case (acc,x)=>acc.enqueue(x)
			}
		}
	}
}
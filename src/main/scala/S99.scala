
import scalaz._
import Scalaz._


object S99 extends App{
  
  def last(l:List[Int]):Int = l match {
    case x :: Nil => x
    case x :: xs => last(xs)
    case _ => throw new Exception("Nil")
  }

  def penultimate(l:List[Int]):Int = l match {
    case x :: y :: Nil => x
    case x :: xs => penultimate(xs)
    case _ => throw new Exception("Nil")
  }

  def nth[T](n:Int, l:List[T]):T = if( n==0) l.head else nth(n-1,l.tail)

  def length(l:List[_]):Int = {
    def proc(n:Int, l:List[_]):Int = l match{
      case Nil => n
      case x :: xs => proc(n+1, xs)
    }
    proc(0, l)
  }

  def reverse[T](l:List[T]):List[T] = l.foldLeft(Nil:List[T])((a,b) => b::a)
    
  def isPalindrome(l:List[_]) = l == reverse(l)

  def flatten(l:List[Any]):List[Any] = {
    def proc(ret:List[Any], x:Any):List[Any] = x match{
      case Nil => ret
      case y::ys => proc(proc(ret,y), ys)
      case _ => ret :+ x
    }
    proc(Nil, l)
  }

  def compress[T](l:List[T]):List[T] = l.foldLeft(List[T]())((xs,x) => xs match{
    case Nil => x :: xs
    case y :: ys if y != x => x :: xs
    case _ => xs
  }).reverse

  def pack[T](l:List[T]):List[List[T]] = l.foldLeft(List[List[T]]())((xs,x) => xs match{
    case Nil => List(x) :: Nil
    case y :: ys if y.head != x => List(x) :: xs
    case y :: ys => (x :: y) :: ys
  }).reverse

  def encode[T](l:List[T]) = pack(l).map(xs=>(xs.size, xs.head))

  def encodeModified[T](l:List[T]) = encode(l).map{
    case (1, x) => x
    case x => x
  }

  def duplicate[T](l:List[T]):List[T] = l <* List(1,2)

  def duplicateN[T](n:Int, l:List[T]):List[T] = l <* List.fill(3)(0)

  def uniq[T](l:List[T]):List[T] = l.foldLeft(List[T]())((xs,x) => xs.find(_==x) match{
    case Some(_) => xs
    case _ =>  xs :+ x
  })

  def run{
    duplicateN(3, List('a, 'b, 'c, 'c, 'd)) assert_=== List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    duplicate(List('a, 'b, 'c, 'c, 'd)) assert_=== List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd) // P14
    assert(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))) // P12
    last(List(1, 1, 2, 3, 5, 8)) assert_=== 8 // P01
    penultimate(List(1, 1, 2, 3, 5, 8)) assert_=== 5 // P02
    nth(2, List(1, 1, 2, 3, 5, 8)) assert_=== 2 // P03
    length(List(1, 1, 2, 3, 5, 8)) assert_=== 6 // P04
    reverse(List(1, 1, 2, 3, 5, 8)) assert_=== List(8, 5, 3, 2, 1, 1) // P05
    isPalindrome(List(1, 2, 3, 2, 1)) assert_=== true // P06
    assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List[Any](1, 1, 2, 3, 5, 8)) // P07
    compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) assert_=== List('a, 'b, 'c, 'a, 'd, 'e)
    pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) assert_=== List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) assert_=== List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    
  }

  run
}

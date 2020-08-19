package iomonad

trait TailRec[A] {
  def flatMap[B](f: A=>TailRec[B]):TailRec[B] = TFlatMap(this,f)
  def map[B](f:A=>B):TailRec[B] = flatMap(f andThen(TReturn(_)))
}
case class TReturn[A](a:A) extends TailRec[A]
case class TSuspend[A](resume: ()=>A) extends TailRec[A]
case class TFlatMap[A,B](sub:TailRec[A],k: A=>TailRec[B]) extends TailRec[B]
object TailRec {
    val f: Int=>TailRec[Int] = (x:Int)=>TReturn(x)
    val g = List.fill(100000)(f).foldLeft(f)((a,b)=>x=>TSuspend(()=>()).flatMap(_=>a(x).flatMap(b)))
    val g2 = List.fill(100000)(f).foldLeft(f)((a,b)=>x=>TFlatMap(a(x),b))
    @annotation.tailrec 
    def run[A](t:TailRec[A]):A = t match {
        case TReturn(a) => a
        case TSuspend(resume) => resume()
        case TFlatMap(sub, k) => sub match {
            case TReturn(a1) => run(k(a1))
            case TSuspend(resume) => run(k(resume()))
            case TFlatMap(sub1, k1) => run(sub1 flatMap(a => k1(a) flatMap(k)))        
        }
    }
}
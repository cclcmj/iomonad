package iomonad

import answer.Par._
import answer._


trait Async[A] {
  def flatMap[B](f: A=>Async[B]):Async[B] = 
    AFlatMap(this,f)
  def map[B](f: A=>B):Async[B] = 
    flatMap(f andThen(AReturn(_)))
}
case class AReturn[A](a:A) extends Async[A]
case class ASyspend[A](resume:Par[A]) extends Async[A]
case class AFlatMap[A,B](sub:Async[A],k: A=>Async[B]) extends Async[B]
object Asyenc {
    @annotation.tailrec
    def step[A](async:Async[A]):Async[A] = async match {
        case AFlatMap(AFlatMap(x,f), g) =>step(x flatMap(a=>f(a) flatMap g))
        case AFlatMap(AReturn(x), k) => step(k(x))
        case _ => async 
    }
    def run[A](async:Async[A]):Par[A] = step(async) match {
        case AReturn(a) => Par.unit(a)
        case ASyspend(resume) => resume
        case AFlatMap(x, f) => x match {
            case ASyspend(resume) => Par.flatMap(resume)(a => run(f(a)))
            case _ => sys.error("Impossible; `step` eliminates these cases")
        }
    }
}

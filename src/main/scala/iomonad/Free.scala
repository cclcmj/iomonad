package iomonad

import monad.Monad
import answer.Nonblocking.Par._
import answer.Nonblocking.Par

trait Free[F[_],A]{
    def map[B](f:A=>B):Free[F,B] = flatMap(f andThen(FReturn(_)))
    def flatMap[B](f:A=>Free[F,B]):Free[F,B] = FFlatMap(this,f) 
}
case class FReturn[F[_],A](a:A) extends Free[F,A]
case class FSuspend[F[_],A](s:F[A]) extends Free[F,A]
case class FFlatMap[F[_],A,B](sub:Free[F,A],f:A=>Free[F,B]) extends Free[F,B]
object Free {
    type FTailRec[A] = Free[Function0,A]
    type FAsync[A] = Free[Par,A]
    def freeMonad[F[_]]:Monad[({type f[a] = Free[F,a]})#f] = new Monad[({type f[a] = Free[F,a]})#f]{
        override def unit[A](a: => A): Free[F,A] = new FReturn(a)
        override def flatMap[A, B](fa: Free[F,A])(f: A => Free[F,B]): Free[F,B] = new FFlatMap(fa,f)
    }
    @annotation.tailrec
    def runTrampoline[A](ff:Free[Function0,A]):A = ff match {
        case FReturn(a) => a
        case FSuspend(s) => s()
        case FFlatMap(sub,f) => sub match {
            case FReturn(a) => runTrampoline(f(a))
            case FSuspend(s1) => runTrampoline(f(s1()))
            case FFlatMap(sub1, f1) => runTrampoline(sub1 flatMap(f1) flatMap f) 
        }
    }
    def step[F[_],A](fa:Free[F,A]):Free[F,A] = fa match {
        case FFlatMap(FReturn(a), f) => step(f(a))
        case FFlatMap(FFlatMap(sub,f), f1) =>  step(sub flatMap(a=>f(a) flatMap(f1)))
        case _ => fa
    }
    def run[F[_],A](a:Free[F,A])(implicit F:Monad[F]):F[A] = step(a) match {
        case FReturn(a) => F.unit(a)
        case FSuspend(s) => s
        case FFlatMap(sub, f) => sub match {
            case FSuspend(s) => F.flatMap(s)(a=>run(f(a)))
            case _ => sys.error("Impossible, since `step` eliminates these cases")
        }
    }
    import iomonad.Translate._
    def runFree[F[_],G[_],A](free:Free[F,A])(t:F~>G)(implicit G:Monad[G]):G[A] = step(free) match {
        case FReturn(a) => G.unit(a)
        case FSuspend(s) => t(s)
        case FFlatMap(FSuspend(r), f) =>  G.flatMap(t(r))(a=>runFree(f(a))(t))
        case _ => sys.error("Impossible; `step` eliminates these cases")
    }
}

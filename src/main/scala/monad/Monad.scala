package monad

import answer.Par._
import answer.Par
import iomonad.Free

trait Functor[F[_]]{
    def map[A,B](a:F[A])(f: A=>B):F[B]
}
trait Monad[F[_]] extends Functor[F]{self =>
    def unit[A](a: =>A):F[A]
    def flatMap[A,B](fa:F[A])(f: A=>F[B]):F[B]

    def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a=>unit(f(a)))
    def map2[A,B,C](fa:F[A],fb:F[B])(f: (A,B)=>C):F[C] = flatMap(fa)(a=>map(fb)(b=>f(a,b)))
    def as[A,B](a:F[A])(b:B):F[B] = map(a)(_=>b)
    def skip[A](a:F[A]):F[Unit] = as(a)(())
    //只有cond函数返回ture 一直循环重复第一个参数的作用
    def doWhile[A](a:F[A])(cond:A=>F[Boolean]):F[Unit] = for {
        a1 <- a
        ok <- cond(a1)
        _ <- if(ok) doWhile(a)(cond) else unit(())
    } yield ()
    // 无限重复参数的作用
    def forever[A,B](a:F[A]):F[B] = {
        lazy val t: F[B] = forever(a)
        a flatMap(_=>t)
    }
    // 使用f折叠流，组合作用并返回结果
    def foldM[A,B](l:Stream[A])(z:B)(f:(B,A)=>F[B]):F[B] = 
        l match {
            case h #:: t => f(z,h) flatMap(z2 => foldM(t)(z2)(f))
            case _ => unit(z)
        }
    // 同上 除了不返回结果
    def foldM_[A,B](l:Stream[A])(z:B)(f:(B,A)=>F[B]):F[Unit] = 
        skip(foldM(l)(z)(f))
    // 对流中每个元素调用函数f并组合作用
    def foreachM[A](l:Stream[A])(f: A=>F[Unit]):F[Unit] = 
        foldM_(l)(())((u,a)=>skip(f(a)))
    implicit def toMonadic[A](a:F[A]):Monadic[F,A] = 
        new Monadic[F,A] {
            val F: Monad[F] = self
            def get: F[A] = a
        }
}
object Monad{
    def freeMonad[F[_]] = new Monad[({type f[a] = Free[F,a]})#f]{
        import iomonad._
        def unit[A](a: => A):Free[F,A] = FReturn(a)
        def flatMap[A, B](fa: Free[F,A])(f: A => Free[F,B]): Free[F,B] = fa flatMap f
    }
    val function0Monad = new Monad[Function0] {
      override def unit[A](a: => A): () => A = ()=>a
      override def flatMap[A, B](fa: () => A)(f: A => (() => B)): () => B = ()=>f(fa())()
    }
    val parMonad = new Monad[Par]{
      override def unit[A](a: => A): Par.Par[A] = Par.unit(a)
      override def flatMap[A, B](a: Par.Par[A])(f: A => Par[B]): Par[B] = Par.fork {Par.flatMap(a)(f)}
    }
}
trait Monadic[F[_],A]{
    val F:Monad[F]
    import F._
    def get: F[A]
    private val a = get
    def map[B](f:A=>B):F[B] = F.map(a)(f)
    def flatMap[B](f:A=>F[B]):F[B] = F.flatMap(a)(f)
}

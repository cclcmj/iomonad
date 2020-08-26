package object iomonad {
  import answer.Nonblocking._
  import iomonad.Free
  import java.util.concurrent._
  import monad.Monad
  type IO[A] = Free[Par,A]
  def IO[A](a: => A) = FSuspend(Par.delay(a))
  def unsafePerformIO[A](io:IO[A])(implicit E:ExecutorService) = 
    Par.run(E){Free.run(io)(Monad.nparMonad)}
}

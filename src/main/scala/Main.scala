import iomonad.TemperatureConverter
import answer.IO2a
import iomonad.IO
import iomonad.TailRec
import iomonad.Console
import monad.Monad
import iomonad.ConsoleReader
import iomonad.FSuspend
import answer.Par
import answer.Par._
import iomonad.Free
import localEffects.STImmutable

object Main extends App {
  // println("start")
  // TemperatureConverter.converter.run
  // val p = IO.forever(IO.printLine("still going..."))
  // IO.run(p)
  // TailRec.g(42)
  // Monad.freeMonad.forever(Console.printLn("Hello"))
  // val c = for {
  //   _ <- Console.printLn("ppppp")
  //   r <- Console.readLn
  //   _ <- Console.printLn(r.getOrElse("error"))
  // } yield ()
  // Console.runConsole(Monad.freeMonad.forever(c))
  // ConsoleReader.runConsoleReader(Monad.freeMonad.forever(c)).run("a")
  Console.runConsole(Console.printLn(STImmutable.quicksort(3::1::2::Nil).toString()))
}
abstract class AppIO {
  import java.util.concurrent._
  
  type IO[A] = Free[Par,A]

  def unsafePerformIO[A](a:IO[A])(pool:ExecutorService):A = 
    Par.run(pool)(Free.run(a)(Monad.parMonad)).get()
  def main(args:Array[String]):Unit = {
    val pool = Executors.newFixedThreadPool(8)
    unsafePerformIO(pureMain(args))(pool)
  }
  def pureMain(args:IndexedSeq[String]):IO[Unit] ={ 
   val c = for{
     _ <- Console.printLn(STImmutable.quicksort(3::1::2::Nil).toString)
   }yield ()
   Console.translate(c)(Console.consoleToPar)
  }
}
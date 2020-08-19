import iomonad.TemperatureConverter
import answer.IO2a
import iomonad.IO
import iomonad.TailRec
import iomonad.Console
import monad.Monad
import iomonad.ConsoleReader
import iomonad.FSuspend

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
  ConsoleReader.runConsoleReader(Monad.freeMonad.forever(c)).run("a")
}
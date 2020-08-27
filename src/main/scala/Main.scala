import localEffects.STImmutable
import java.util.concurrent._
import iomonad._
import streamingIO.CountLines
object AppIO {
  def main(args:Array[String]):Unit = {
    val pool = Executors.newFixedThreadPool(8)
    unsafePerformIO(pureMain(args))(pool)
  }
  def pureMain(args:IndexedSeq[String]):IO[_] ={ 
    //val c = for{
    //  _ <- Console.printLn(STImmutable.quicksort(3::1::2::Nil).toString)
    //}yield ()
    //Console.translate(c)(Console.consoleToPar)
    CountLines.linesGt40k(" ")
  }
}
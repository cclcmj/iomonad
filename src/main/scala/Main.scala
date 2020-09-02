import localEffects.STImmutable
import java.util.concurrent._
import iomonad._
import streamingIO._
object AppIO {
  def main(args:Array[String]):Unit = {
    val pool = Executors.newFixedThreadPool(8)
    unsafePerformIO(pureMain(args))(pool)
  }
  def pureMain(args:IndexedSeq[String]):IO[_] ={ 
    val p = Process.lift((a:Double)=>a).zipwithIndex (Stream(1.0,2.0,3.0,4.0))
    val c = for{
      _ <- Console.printLn(p.toList.toString)
    } yield ()
    Console.translate(c)(Console.consoleToPar)
  }
}
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
    val p = Process.dropWhile[Int](i=>i%2==0)(Stream(1,2,3,4))
    val c = for{
      _ <- Console.printLn(p.toList.toString)
    } yield ()
    Console.translate(c)(Console.consoleToPar)
  }
}
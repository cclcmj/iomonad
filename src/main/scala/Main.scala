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
    val p1 = Process.lift((d:Double)=>d.toString()+"p1p1p1p1p1") 
    val f1 =  (s:String)=>Process.lift((d:Double)=>s+d.toString+"f1f1f1f1f1f1")
    val f2 =  (s:String)=>Process.lift((d:Double)=>s+d.toString+"f2f2f2f2f2f2")
    val resultP = (p1 flatMap f1 flatMap f2) (Stream(1.0,4.0,6.0,7.0))
    val c =  Console.printLn(resultP.toList.toString())
    Console.translate(c)(Console.consoleToPar)
  }
}
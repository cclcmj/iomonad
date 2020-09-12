package streamingIO
import EProcess._
import EProcess1._
import Tee._
import java.io.FileWriter
import iomonad.IO
/*
 *使用EProcess类型进行输出
 *可以将去向(sink)看作递送函数的过程
 */
object Sink {
  type Sink[F[_],O] = EProcess[F,O=>EProcess[F,Unit]]
  /*
   *将一个字符串写入文件的sink
   */
  def fileW(file:String,append:Boolean = false):Sink[IO,String] = 
    resource[FileWriter,String => EProcess[IO,Unit]]
    {IO{new FileWriter(file,append)}}
    {w => constant{(s:String)=>eval[IO,Unit](IO(w.write(s)))}}
    {w => eval_[IO,Unit,String=>EProcess[IO,Unit]](IO(w.close))}
  /*
   *无限的常量流
   */
  def constant[A](a:A):EProcess[IO,A] = 
    eval[IO,A](IO(a)).repeat
  /*
   *将输出传给sink
   */
  implicit class EProcessToSink[F[_],O](p:EProcess[F,O]) {
    def to[O2](sink:Sink[F,O]):EProcess[F,Unit] = join{(p tee sink)(Tee.zipWith((o,f)=>f(o)))}
    def drain[O2]:EProcess[F,O2] = p match {
      case Halt(e) => Halt(e)
      case Emit(head,tail) => tail.drain
      case Await(req,recv) => Await(req,recv andThen(_.drain))
    }
  }
  def join[F[_],O](p:EProcess[F,EProcess[F,O]]):EProcess[F,O] = p.flatMap(pa=>pa)
  /*
   *应用例子
   */
  /** Emits `sep` in between each input received. */
  def intersperse[I](sep: I): EProcess1[I,I] =
      await1[I,I](i => emit1(i) ++ id.flatMap(i => emit1(sep) ++ emit1(i)))
  def id[I]: EProcess1[I,I] =
      await1((i: I) => emit(i, id))
  val converter:EProcess[IO,Unit] = 
    lines("/home/vvvkvmj/lines.txt")
    .|>(intersperse("\n"))
    .to(fileW("/home/vvvkvmj/celsius.txt"))
    .drain
}

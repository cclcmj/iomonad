package streamingIO

import streamingIO.EProcess.Halt
import iomonad.IO
import scala.collection.immutable.{IndexedSeq}
import java.io.{BufferedReader,FileReader}
import monad.Monad

//ExtensableProcess:可扩展的Process
trait EProcess[F[_],O]{
    import EProcess._
    //调用p.onHalt(f)为了在p结束时对Halt(e)中的e执行f（e），这样做允许我们提供扩展逻辑以根据终止的原因来做合适的操作
    //帮助函数Try，可以保证Process求值的安全性，将捕获的任意异常转换成Halt，这对资源安全来说非常重要
    def onHalt(f:Throwable=>EProcess[F,O]):EProcess[F,O] = this match {
        case Halt(e) => Try(f(e))
        case Emit(h,t) => Emit(h,t.onHalt(f))
        case Await(req,recv)=> Await(req,recv andThen(_.onHalt(f)))
    }
    def ++(p: =>EProcess[F,O]):EProcess[F,O] = 
        this.onHalt{
            case End => p
            case err => Halt(err)
        }
    def flatMap[O2](f:O=>EProcess[F,O2]):EProcess[F,O2] = this match {
        case Halt(err) => Halt(err)
        case Emit(head, tail) => Try(f(head)) ++ tail.flatMap(f)
        case Await(req, recv) => Await(req,recv andThen(_ flatMap f))
    }
    //更通用的runLog
    //注：这里的实现是没法进行尾递归优化的，完全依赖底层的Monad来保证栈安全
    def runLog(implicit F:MonadCatch[F]):F[IndexedSeq[O]] = {
        def go(cur:EProcess[F,O],acc:IndexedSeq[O]):F[IndexedSeq[O]] = 
            cur match {
                case Emit(head, tail) => go(tail,acc :+ head)
                case Halt(End) => F.unit(acc)
                case Halt(err) => F.fail(err)
                case Await(req, recv) => F.flatMap(F.attempt(req)) {e=> go(Try(recv(e)),acc)}
            }
        go(this,IndexedSeq())
    }
    //保证资源安全
    def onComplete(p: => EProcess[F,O]):EProcess[F,O] = 
        this.onHalt {
            case End => p.asFinalizer
            case err => p.asFinalizer ++ Halt(err)
        }
    //工具函数
    def asFinalizer:EProcess[F,O] = this match {
        case Emit(head, tail) => Emit(head,tail.asFinalizer)
        case Halt(err) => Halt(err)
        case Await(req, recv) => await(req) {
            case Left(Kill) => this.asFinalizer
            case x => recv(x) 
        }
    }
    //重复
    def repeat:EProcess[F,O] = 
        this ++ this.repeat
}
object EProcess{
    case class Await[F[_],A,O](
        req:F[A],
        recv:Either[Throwable,A]=>EProcess[F,O]
        //一旦执行req时出现了错误，recv就可以决定如何做
        //recv函数应该是以trampolined的方式返回TailRec[Process[F,O]]从而避免栈溢出的问题
    ) extends EProcess[F,O]
    case class Emit[F[_],O](
        head:O,
        tail:EProcess[F,O]
    ) extends EProcess[F,O]
    //致使停止的err可能是错误，也可能是End正常结束
    case class Halt[F[_],O](err:Throwable) extends EProcess[F,O]
    //这个Exception表示正常结束，用来进行流程控制
    case object End extends Exception
    //这个Exception强制结束，后面用得到,这个状态此后需要清理任何使用过的资源
    case object Kill extends Exception
    //帮助函数Try
    def Try[F[_],O](p: =>EProcess[F,O]):EProcess[F,O] = 
        try p
        catch{case e:Throwable =>Halt(e)}
    def await[F[_],A,O](req:F[A])(recv:Either[Throwable,A]=>EProcess[F,O]):EProcess[F,O] = Await(req,recv)
    def emit[F[_],O](
        head: O,
        tail: EProcess[F,O] = Halt[F,O](End)): EProcess[F,O] =
      Emit(head, tail)
    //通用组合子eval将F[A]升格为仅传递（emit）F[A]结果的Process
    //eval_升格一个F[A]为无传递值的EProcess
    def eval[F[_],A](a:F[A]):EProcess[F,A] = await[F,A,A](a){
        case Left(err) => Halt(err)
        case Right(value) => Emit(value,Halt(End))
    }
    def eval_[F[_],A,B](a:F[A]):EProcess[F,B] = 
        drain(eval[F,A](a))
    /*
     *目的是忽略Process的所有输出？？？？
     */
    def drain[F[_],O,O2](p:EProcess[F,O]):EProcess[F,O2] = p match {
        case Halt(e) => Halt(e)
        case Emit(head,tail) => drain(tail)
        case Await(req,recv) => Await(req,recv andThen (drain(_)))
    }
    //来源
    def runLog[O](src:EProcess[IO,O]):IO[IndexedSeq[O]] = IO{
        import iomonad._
        val E = java.util.concurrent.Executors.newFixedThreadPool(4)
        @annotation.tailrec
        def go(cur:EProcess[IO,O],acc:IndexedSeq[O]):IndexedSeq[O] = 
            cur match {
                case Emit(head, tail) => go(tail,acc:+head)
                case Halt(End) => acc
                case Halt(err) => throw err
                case Await(req,recv) => 
                    val next = try recv(Right(unsafePerformIO(req)(E)))
                                catch {case e:Throwable => recv(Left(e))}
                    go(next,acc)
            }
            try go(src,IndexedSeq())
            finally E.shutdown()
    }
    //遍历文件所有行
    val p:EProcess[IO,String] = await[IO,BufferedReader,String](IO(new BufferedReader(new FileReader("/home/vvvkvmj/lines.txt")))){
        case Right(b) => 
            lazy val next :EProcess[IO,String] = await[IO,String,String](IO(b.readLine)) {
                case Left(e) => await[IO,Unit,String](IO(b.close()))(_ => Halt(e))
                case Right(line) => if(line eq null) Halt(End)
                                    else Emit(line,next)
            }
            next
        case Left(e) => Halt(e)
    }
    //更通用的runLog辅助
    trait MonadCatch[F[_]] extends Monad[F] {
        def attempt[A](a:F[A]):F[Either[Throwable,A]]
        def fail[A](t:Throwable):F[A]
    }
    //基于onComplete和asFinalizer船舰一个资源安全的基于文件Process[I,O]
    def resource[R,O](acquire:IO[R])
        (use: R=>EProcess[IO,O])
        (release: R=>EProcess[IO,O]):EProcess[IO,O] = 
            await[IO,R,O](acquire){
                case Right(r) => use(r).onComplete(release(r))
                case Left(err) => Halt(err) 
            }
    //一个大文件的所有行lines:Process[IO,String]
    def lines(filename:String):EProcess[IO,String] = 
        resource
            {IO(io.Source.fromFile(filename))}
            {
                src=>
                    lazy val iter = src.getLines()
                    def step = if(iter.hasNext) Some(iter.next()) else None
                    lazy val lines :EProcess[IO,String] = eval[IO,Option[String]](IO(step)).flatMap{
                        case None => Halt(End)
                        case Some(line) => Emit(line,lines)
                    }
                    lines
            }
            {src => eval_[IO,Unit,String](IO(src.close))}

}

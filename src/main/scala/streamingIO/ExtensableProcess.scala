package streamingIO

import streamingIO.ExtensableProcess.Halt

trait ExtensableProcess[F[_],O]{
    import ExtensableProcess._
    //调用p.onHalt(f)为了在p结束时对Halt(e)中的e执行f（e），这样做允许我们提供扩展逻辑以根据终止的原因来做合适的操作
    //帮助函数Try，可以保证Process求值的安全性，将捕获的任意异常转换成Halt，这对资源安全来说非常重要
    def onHalt(f:Throwable=>ExtensableProcess[F,O]):ExtensableProcess[F,O] = this match {
        case Halt(e) => Try(f(e))
        case Emit(h,t) => Emit(h,t.onHalt(f))
        case Await(req,recv)=> Await(req,recv andThen(_.onHalt(f)))
    }
    def ++(p: =>ExtensableProcess[F,O]):ExtensableProcess[F,O] = 
        this.onHalt{
            case End => p
            case err => Halt(err)
        }
    def Try[F[_],O](p: =>ExtensableProcess[F,O]):ExtensableProcess[F,O] = 
        try p
        catch{case e:Throwable =>Halt(e)}
    def flatMap[O2](f:O=>ExtensableProcess[F,O2]):ExtensableProcess[F,O2] = this match {
        case Halt(err) => Halt(err)
        case Emit(head, tail) => Try(f(head)) ++ tail.flatMap(f)
        case Await(req, recv) => Await(req,recv andThen(_ flatMap f))
    }
}
object ExtensableProcess{
    case class Await[F[_],A,O](
        req:F[A],
        recv:Either[Throwable,A]=>ExtensableProcess[F,O]
        //一旦执行req时出现了错误，recv就可以决定如何做
        //recv函数应该是以trampolined的方式返回TailRec[Process[F,O]]从而避免栈溢出的问题
    ) extends ExtensableProcess[F,O]
    case class Emit[F[_],O](
        head:O,
        tail:ExtensableProcess[F,O]
    ) extends ExtensableProcess[F,O]
    //致使停止的err可能是错误，也可能是End正常结束
    case class Halt[F[_],O](err:Throwable) extends ExtensableProcess[F,O]
    //这个Exception表示正常结束，用来进行流程控制
    case object End extends Exception
    //这个Exception强制结束，后面用得到,这个状态此后需要清理任何使用过的资源
    case object Kill extends Exception
    //test proxy
}

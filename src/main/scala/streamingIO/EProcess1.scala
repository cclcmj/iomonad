package streamingIO

import streamingIO.EProcess.Await
import streamingIO.EProcess.Emit
import streamingIO.EProcess.End
import streamingIO.EProcess.Halt

object EProcess1 {
    import EProcess._
    //为了表现Process1[I，O],弄出一个恰当的F，让它只能用Process为I类型的元素制造请求
    case class Is[I]() {
        sealed trait f[X]
        val Get = new f[I]{}
    }
    type EProcess1[I,O] = EProcess[Is[I]#f,O]
    def await1[I,O](
        recv:I => EProcess1[I,O],
        fallback: EProcess1[I,O] = halt1[I,O]
        ):EProcess1[I,O] = 
            Await(Is[I]().Get,(e:Either[Throwable,I]) => e match {
                case Left(End) => fallback
                case Left(err) => Halt(err)
                case Right(value) =>  Try(recv(value)) 
            })
    def emit1[I,O](h:O,t1:EProcess1[I,O] = halt1[I,O]):EProcess1[I,O] = Emit(h,t1)
    def halt1[I,O]:EProcess1[I,O] = Halt[Is[I]#f,O](End)
    def lift[I,O](f: I=>O):EProcess1[I,O] = 
        await1[I,O](i => emit1(f(i))) repeat
    def filter[I](f:I=>Boolean):EProcess1[I,I] = 
        await1[I,I](i=>if(f(i)) emit1(i) else halt1) repeat
    implicit class EProcessAdd[F[_],O](p:EProcess[F,O]){
        def |>[O2](p2:EProcess1[O,O2]):EProcess[F,O2] = {
            p2 match {
                case Halt(e) => p.kill onHalt {e2=>Halt(e) ++Halt(e2)}
                case Emit(head,tail) => Emit(head,p |> tail)
                case Await(req,recv) => p match {
                    case Halt(err) => Halt(err) |> recv(Left(err))
                    case Emit(head,tail) => tail |> Try(recv(Right(head)))
                    case Await(req0,recv0) => await(req0)(recv0 andThen(_ |> p2))
                }
            }
        }
        @annotation.tailrec
        final def kill[O2]:EProcess[F,O2] = p match {
            case Await(req,recv) => drain(recv(Left(Kill))).onHalt{
                case Kill => Halt(End)
                case e => Halt(e)
            }
            case Halt(e) => Halt(e)
            case Emit(h,t) => t.kill
        }
    }
}

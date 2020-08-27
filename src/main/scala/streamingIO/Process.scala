/*
 * @Author: mj
 * @Date: 2020-08-26 07:32:18
 * @LastEditTime: 2020-08-27 09:24:11
 * @LastEditors: Please set LastEditors
 * @Description: In User Settings Edit
 * @FilePath: /iomonad/src/main/scala/streamingIO/Process.scala
 */
package streamingIO
/*
 * Process 可以将I类型的流转换为O的流，但并非是一个简单的函数流Stream[I]=>Stream[O]
 * 取而代之的是一个驱动器驱动的状态机，和一个同时接收Process和输入流的函数
 * 一个Process可以有三种种状态：Emit、Await、Halt。（每个都是驱动的信号）
*/
sealed trait Process[I,O] {
    //apply是Process与流的结合，是三种状态的Process处理每一个流中的元素
    //即Process(h(下一个元素),t(下一个Process))(s)转变为f(h)#::t(s)
    def apply(s:Stream[I]):Stream[O] ={ this match {
        case Await(recv) => s match {
            case h#::t => recv(Some(h))(t)
            case xs => recv(None)(xs)//stream 是空的
        }
        case Emit(h, t) => h #:: t(s)
        case Halt() => Stream()
        }
    }
    // repeat 是由执行前一个Process转换为执行下一个Process的工具
    //与liftone相结合 做到
    //go(await(f))(s)=>emit(f(h),go(halt()))(s)
    //=>fh#::go(await(f))(None)=>fh#::halt()(None)结束
    def repeat:Process[I,O] = {
        def go(p:Process[I,O]):Process[I,O] = p match {
            case Halt() => go(this)
            case Await(recv) => Await{
                case None => recv(None)
                case i => go(recv(i))
            }
            case Emit(h, t) => Emit(h,go(t))
        }
        go(this)
    }
}
object Process{
    //将f提升为Process，添加状态转换逻辑
    def liftOne[I,O](f:I=>O):Process[I,O] = Await{
        case Some(value) => Emit(f(value))
        case None => Halt()
    }
    def lift[I,O](f:I=>O):Process[I,O] = liftOne(f).repeat
}
//Emit告诉驱动器将head值递送给输出流，而后tail的部分继续由状态机处理
case class Emit[I,O](head:O,tail:Process[I,O] = Halt[I,O]()) extends Process[I,O]
//Await请求从输入流得到下一个值，驱动器则将下一个值传递给函数recv，一旦输入流再无元素则给None
case class Await[I,O](recv:Option[I]=>Process[I,O]) extends Process[I,O]
//Halt告诉驱动器暂时没有任何元素要从输入流里读取或递送给输出流
case class Halt[I,O]() extends Process[I,O]

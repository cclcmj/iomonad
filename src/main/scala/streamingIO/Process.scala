/*
 * @Author: mj
 * @Date: 2020-08-26 07:32:18
 * @LastEditTime: 2020-08-26 08:08:43
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
    def apply(s:Stream[I]):Stream[O] = this match {
        case Await(recv) => s match {
            case h#::t => recv(Some(h))(t)
            case xs => recv(None)(xs)//stream 是空的
        }
        case Emit(h, t) => h #:: t(s)
        case Halt() => Stream()
    }
}
//Emit告诉驱动器将head值递送给输出流，而后tail的部分继续由状态机处理
case class Emit[I,O](head:O,tail:Process[I,O] = Halt[I,O]()) extends Process[I,O]
//Await请求从输入流得到下一个值，驱动器则将下一个值传递给函数recv，一旦输入流再无元素则给None
case class Await[I,O](recv:Option[I]=>Process[I,O]) extends Process[I,O]
//Halt告诉驱动器暂时没有任何元素要从输入流里读取或递送给输出流
case class Halt[I,O]() extends Process[I,O]

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
    import Process._
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
    //--------------组合和追加处理------------
    //组合Process值来构建更复杂的转化流。
    //给定两个Process值f和g，实现f的输出作为g的输入
    def |>[O2](p2:Process[O,O2]):Process[I,O2] = {
        case Halt() => Halt()
        case Emit(h,t)=>
    }
}
object Process{
    //Emit告诉驱动器将head值递送给输出流，而后tail的部分继续由状态机处理
    case class Emit[I,O](head:O,tail:Process[I,O] = Halt[I,O]()) extends Process[I,O]
    //Await请求从输入流得到下一个值，驱动器则将下一个值传递给函数recv，一旦输入流再无元素则给None
    case class Await[I,O](recv:Option[I]=>Process[I,O]) extends Process[I,O]
    //Halt告诉驱动器暂时没有任何元素要从输入流里读取或递送给输出流
    case class Halt[I,O]() extends Process[I,O]
    //将f提升为Process，添加状态转换逻辑
    def liftOne[I,O](f:I=>O):Process[I,O] = Await{
        case Some(value) => Emit(f(value))
        case None => Halt()
    }
    def lift[I,O](f:I=>O):Process[I,O] = liftOne(f).repeat
    //过滤
    def filter[I](p:I=>Boolean):Process[I,I] = Await[I,I] {
        case Some(value) if(p(value)) => Emit(value)
        case _ => Halt()
    }.repeat
    //求和
    def sum:Process[Double,Double] = {
        def go(acc:Double):Process[Double,Double] = Await {
            case Some(value) => Emit(value+acc,go(value+acc))
            case None => Halt()
        }
        go(0.0)
    }
    def id[I]:Process[I,I] = lift(identity)
    //获取给定数量的元素然后就停止
    def take[I](n:Int):Process[I,I] = 
        if (n<=0) Halt()
        else Await{
                case Some(value) => Emit(value,take(n-1))
                case None => Halt()
            }
    //实现丢弃给定数量的元素并将剩余的返回
    def drop[I](n:Int):Process[I,I] = Await{
                case Some(value) if(n>0) => drop(n-1)
                case Some(value) => Emit(value,drop(n-1))
                case None => Halt()
            }
    def dropAnswer[I](n:Int):Process[I,I] = 
            if (n<=0) id
            else Await{
                case Some(value) => dropAnswer(n-1)
                case None => Halt()
            }
    //仅获取条件参数为真的元素
    def takeWhile[I](f: I=>Boolean):Process[I,I] = Await{
        case Some(value) if(f(value))=> Emit(value,takeWhile(f))
        case Some(value) => takeWhile(f)
        case None => Halt() 
    }
    //丢弃条件参数为真的元素
    def dropWhile[I](f: I=>Boolean):Process[I,I] = Await {
        case Some(value) if(f(value))=> dropWhile(f)
        case Some(value) => Emit(value,dropWhile(f))
        case None => Halt()
    }
    //实现count，计算元素的个数
    def count[I]:Process[I,Int] = {
        def go(num:Int):Process[I,Int] = Await {
            case Some(value) => Emit(num+1,go(num+1))
            case None => Halt()
        }
        go(0)
    }
    //求平均值
    def mean:Process[Double,Double] = {
        def go(sum:Double,num:Int):Process[Double,Double] = Await{
            case Some(value) => Emit((sum+value)/(num+1),go(sum+value,num+1))
            case None => Halt()
        }
        go(0.0,0)
    }
    //抽象出sum,count,mean共同的模式成为组合子
    //其中每个都有自己的状态，然后根据输入更新状态并输出
    def loop[S,I,O](z:S)(f:(I,S)=>(O,S)):Process[I,O] = Await {
        case Some(value) => Emit(f(value,z)._1,loop(f(value,z)._2)(f))
        case None => Halt()
    }
    //用loop实现sum、count
    def sumLoop:Process[Double,Double] = 
        loop(0.0)((i,s)=>(i+s,i+s))
    def countLoop[I]:Process[I,Int] = 
        loop(0)((_:I,s)=>(s+1,s+1))
    def meanLoop:Process[Double,Double] = 
        loop((0.0,0.0))((i,s)=>((s._1+i)/(s._2+1),(s._1+i,s._2+1)))
}

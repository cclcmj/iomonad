/*
 * @Author: mj
 * @Date: 2020-08-26 07:32:18
 * @LastEditTime: 2020-09-02 16:03:36
 * @LastEditors: Please set LastEditors
 * @Description: In User Settings Edit
 * @FilePath: /iomonad/src/main/scala/streamingIO/Process.scala
 */
package streamingIO

import monad.Monad
import java.io.File
import iomonad.IO
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
        p2 match {
            case Halt() => Halt()
            case Emit(h,t) => Emit(h,this |> t)
            case Await(f) => this match {
                case Halt() => Halt() |> f(None)
                case Emit(h,t) => t |> f(Some(h))
                case Await(recv) => Await((i:Option[I])=>recv(i)|>p2)
            }
        }
    }
    //实现map、flatMap
    def map[O2](f:O=>O2):Process[I,O2] = 
        this match {
            case Halt() => Halt()
            case Emit(head, tail) => Emit(f(head),tail map f)
            case Await(recv) => Await(recv andThen (_ map f))
        }
    def ++(p: =>Process[I,O]):Process[I,O] = this match {
        case Halt() => p
        case Emit(head, tail) => Emit(head,tail ++ p)
        case Await(recv) => Await(recv andThen(_ ++ p))
    }
    def flatMap[O2](f:O=>Process[I,O2]):Process[I,O2] = this match {
        case Halt() => Halt()
        case Emit(head, tail) => f(head) ++ tail.flatMap(f)
        case Await(recv) => Await(recv andThen(_ flatMap f))
    }
    //实现zipWithIndex
    def zip[O2](p:Process[I,O2]): Process[I,(O,O2)] = 
        Process.zip(this,p)
    def zipwithIndex:Process[I,(O,Int)] = 
        this zip countLoop
    
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
    //-----------组合和追加处理-------------
    //Process[I,_]算是一个monad
    def monad[I]:Monad[({type f[x] = Process[I,x]})#f] = 
        new Monad[({type f[x] = Process[I,x]})#f] {
            def unit[A](a: => A): Process[I,A] = Emit(a)
            def flatMap[A, B](fa: Process[I,A])(f: A => Process[I,B]): Process[I,B] = fa flatMap f
        }
    //构建zipWithIndex
    def zip[A,B,C](p1:Process[A,B],p2:Process[A,C]):Process[A,(B,C)] = 
        (p1,p2) match {
            case (Halt(),_) => Halt()
            case (_,Halt()) => Halt()
            case (Emit(b,t1),Emit(c,t2)) => Emit((b,c),zip(t1,t2))
            case (Await(recv),_) => Await((oa:Option[A])=> zip (recv(oa),feed(oa)(p2)))
            case (_,Await(recv)) => Await((oa:Option[A])=> zip(feed(oa)(p1),recv(oa)))
        }
    def feed[A,B](oa:Option[A])(p:Process[A,B]):Process[A,B] = 
        p match {
            case Halt() => p
            case Emit(head, tail) => Emit(head,feed(oa)(tail))
            case Await(recv) => recv(oa)
        }
    //用已有的组合子求平均值
    def mean2 = (sumLoop zip countLoop) |> lift{case (s,c)=> s/c}
    //用已有的组合子实现exists
    def any:Process[Boolean,Boolean] = loop(false)((b:Boolean,s)=>(s||b,s||b))
    def exists[I](f:I=>Boolean):Process[I,Boolean] = lift(f) |> any
    //处理文件
    def processFile[A,B](f:File,p:Process[String,A],z:B)(g:(B,A)=>B):IO[B] = IO{
        @annotation.tailrec
        def go(ss:Iterator[String],cur:Process[String,A],acc:B):B = 
            cur match {
                case Halt() => acc
                case Await(recv) => {
                    val next = if(ss.hasNext) recv(Some(ss.next))
                                else recv(None)
                    go(ss,next,acc)
                }
                case Emit(head, tail) => go(ss,tail,g(acc,head))
            }
        val s = io.Source.fromFile(f)
        try go(s.getLines(),p,z)
        finally s.close()
    }
}

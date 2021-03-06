/*
 * @Author: mj
 * @Date: 2020-08-11 09:40:38
 * @LastEditTime: 2020-08-12 10:11:45
 * @LastEditors: Please set LastEditors
 * @Description: 函数式输入输出函子类
 * @FilePath: /iomonad/src/main/scala/iomonad/IO.scala
 */
package iomonad

import monad.Monad
import scala.annotation.tailrec

trait IOf[A] {
    def map[B](f:A=>B):IOf[B] = 
        flatMap(f andThen(IOReturn(_)))
    def flatMap[B](f:A=>IOf[B]):IOf[B] = 
        IOFlatMap(this,f)
}
object IO extends Monad[IOf]{
    def unit[A](a: => A): IOf[A] = IOReturn(a)
    def flatMap[A, B](fa: IOf[A])(f: A => IOf[B]): IOf[B] = fa flatMap f
    def apply[A](a: => A): IOf[A] = unit(a)
    def printLine(s:String): IOf[Unit] = IOSuspend(()=>IOReturn(println(s)))
    @annotation.tailrec
    def run[A](io:IOf[A]):A = io match {
        case IOReturn(a) => a
        case IOSuspend(resume) => resume()
        case IOFlatMap(sub, k) => sub match {
            case IOReturn(a) => run(k(a))
            case IOSuspend(resume) => run(k(resume()))
            case IOFlatMap(sub1, g) => run(sub1 flatMap(a => g(a) flatMap k))
        }
    }
}
/**
 * @description: 没有其他步骤并立即返回A的纯计算，但run方法遇到此结构体时，它知道计算已经结束了
 * @param {type} 
 * @return {type} 
 */
case class IOReturn[A](a:A) extends IOf[A]
/**
 * @description: 计算暂停，这里resume不接受任何参数并作用产生结果
 * @param {type} 
 * @return {type} 
 */
case class IOSuspend[A](resume: () => A) extends IOf[A]
/**
 * @description: 两个步骤的组合，flatmap具化为一个数据类型而不是函数。当run遇到它时，首先会处理自己算sub并档期返回后继续计算k
 * @param {type} 
 * @return {type} 
 */
case class IOFlatMap[A,B](sub:IOf[A],k: A=>IOf[B]) extends IOf[B]

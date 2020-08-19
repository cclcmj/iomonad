/*
 * @Author: mj
 * @Date: 2020-08-14 10:41:32
 * @LastEditTime: 2020-08-16 09:22:17
 * @LastEditors: Please set LastEditors
 * @Description: 一个支持控制台I/O的monad
 * @FilePath: /iomonad/src/main/scala/iomonad/Console.scala
 */
package iomonad

import answer.Par
import answer.Par._
import scala.io.StdIn
import monad.Monad

trait Console[A] {
  /**
   * @description: 将Console解释为Par[A]
   */  
  def toPar:Par[A]
  /**
   * @description: 将Console解释为Function0[A]
   */  
  def toThunk:()=>A
}
case object ReadLine extends Console[Option[String]]{
  override def toPar = Par.lazyUnit(run)
  override def toThunk: () => Option[String] = ()=>run
/**
 * @description: 帮助函数，用于ReadLine的解释器
 */  
  def run():Option[String] = 
    try Some(StdIn.readLine)
    catch {case e:Exception => None}
}
case class  PrintLine(line:String) extends Console[Unit]{
  override def toPar = Par.lazyUnit(println(line))
  override def toThunk: () => Unit = () => println(line)
}
object Console{
  import Free._
  import Translate._
  type ConsoleIO[A] = Free[Console,A]
  def readLn:ConsoleIO[Option[String]] = FSuspend(ReadLine)
  def printLn(line:String):ConsoleIO[Unit] = FSuspend(PrintLine(line))
  val consoleToFunction0 = new (Console~>Function0){def apply[A](a: Console[A]): () => A = a.toThunk}
  val consoleToPar = new (Console~>Par) {def apply[A](a: Console[A]): Par.Par[A] = a.toPar}
  def runConsoleFunction0[A](a:Free[Console,A]):()=>A = 
    runFree[Console,Function0,A](a)(consoleToFunction0)(Monad.function0Monad)
  def runConsolePar[A](a:Free[Console,A]):Par[A] = runFree(a)(consoleToPar)(Monad.parMonad)
  /**
   * @description: runConsoleFunction0不是栈安全的，因为flatmap对function不是栈安全的
   */  
  def translate[F[_],G[_],A](f:Free[F,A])(fg:F~>G):Free[G,A] = {
    type FreeG[A] = Free[G,A]
    val t = new (F~>FreeG){def apply[A](f: F[A]): FreeG[A] = FSuspend(fg(f))}
    runFree(f)(t)(Monad.freeMonad[G])
  }
  /**
   * @description: 栈安全版本的Console解释器
   */
  def runConsole[A](a:Free[Console,A]):A = 
    runTrampoline(translate(a)(consoleToFunction0))
}
/**
 * @description: F[A]到G[A]的转化类型
 */    
trait Translate[F[_],G[_]] {def apply[A](f:F[A]):G[A]}
/**
 * @description: 这样可以使用中缀语法F~>G来表示Translate[F,G]
 */                                                                                          
object Translate {type ~>[F[_],G[_]] = Translate[F,G]}
/**
 * @description: 将Console动作幻化为纯值并不执行IO，为此将Console转化为一个String=>A，这个函数在A里构建了一个Monad
 */
case class ConsoleReader[A](run:String=>A){
  def map[B](f:A=>B):ConsoleReader[B] = ConsoleReader(r=>f(run(r)))
  def flatMap[B](f:A=>ConsoleReader[B]):ConsoleReader[B] = ConsoleReader(r=>f(run(r)).run(r))
}

object ConsoleReader {
  import Free._
  import Translate._
  import Console._
  /**
  * @description: 用类型类模式为Console增添功能:Reader
  */  
  implicit class ConsoleR[A](console:Console[A]){
    def toReader:ConsoleReader[A] = ConsoleReader(a=>console.toThunk())
  }
  implicit val consoleRMonad= new Monad[ConsoleReader] {
    def unit[A](a: => A): ConsoleReader[A] = ConsoleReader(_=>a)
    def flatMap[A, B](fa: ConsoleReader[A])(f: A => ConsoleReader[B]): ConsoleReader[B] = fa flatMap f
  }
  val consoleToReader = new (Console~>ConsoleReader) {
      def apply[A](a:Console[A]) = a.toReader
    }
  def runConsoleReader[A](io:ConsoleIO[A]):ConsoleReader[A] = 
    runFree[Console,ConsoleReader,A](io)(consoleToReader)
}

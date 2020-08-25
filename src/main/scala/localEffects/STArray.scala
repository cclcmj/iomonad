/*
 * @Author: mj
 * @Date: 2020-08-25 01:05:34
 * @LastEditTime: 2020-08-25 02:57:20
 * @LastEditors: Please set LastEditors
 * @Description: In User Settings Edit
 * @FilePath: /iomonad/src/main/scala/localEffects/STArray.scala
 */
package localEffects

import scala.reflect.ClassTag

sealed abstract class  STArray[S,A:ClassTag]{
  protected def value:Array[A]
  def size:ST[S,Int] = ST(value.size)
  def write(i:Int,a:A):ST[S,Unit] = new ST[S,Unit] {
      protected def run(s: S): (Unit, S) = {
          value(i) = a
          ((),s)
        }
    }
    def read(i:Int):ST[S,A] = ST(value(i))
    def freeze:ST[S,List[A]] = ST(value.toList)
    def fill(xs:Map[Int,A]):ST[S,Unit] = {
        xs.foldRight(ST[S,Unit](())){
            case ((i,a),b) => b flatMap (_ => write(i,a)) 
        }
    }
    def swap(i:Int,j:Int):ST[S,Unit] = for {
        x <- read(i)
        y <- read(j)
        _ <- write(i,y)
        _ <- write(j,x)
    } yield ()
}
object STArray {
    def apply[S,A:ClassTag](sz:Int,v:A):ST[S,STArray[S,A]] = 
        ST(new STArray[S,A] {
            lazy val value = Array.fill(sz)(v)
        })
    def fromList[S,A:ClassTag](xs:List[A]):ST[S,STArray[S,A]] = 
        ST(new STArray[S,A] {
            lazy val value = xs.toArray
        })
    }
 //纯函数的 ip-place 快排
object STImmutable {
    def noop[S] = ST[S,Unit](())
    def partition[S](arr:STArray[S,Int],n:Int,r:Int,pivot:Int):ST[S,Int] = for{
        vp <- arr.read(pivot)
        _ <- arr.swap(pivot,r)
        j <- STRef(1)
        _ <- (1 until r).foldLeft(noop[S]){
            (s,i) => for{
                _ <- s
                vi <- arr.read(i)
                _ <- if(vi < vp) for{
                    vj <- j.read
                    _ <- arr.swap(i,vj)
                    _ <- j.write(vj+1)
                } yield () else noop[S]
            } yield ()}
        x <- j.read
        _ <- arr.swap(x,r)
        } yield x
    def qs[S](a:STArray[S,Int],n:Int,r:Int):ST[S,Unit] = if(n < r) for{
        pi <- partition(a,n,r,n+(r-1)/2)
        _ <- qs(a,n,pi-1)
        _ <- qs(a,pi+1,r)
    } yield () else noop[S]
    def quicksort(xs:List[Int]):List[Int] = 
        if(xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]]{
            def apply[S] = for{
                arr <- STArray.fromList(xs)
                size <- arr.size
                _ <- qs(arr,0,size-1)
                sorted <- arr.freeze
            } yield sorted
        })
}

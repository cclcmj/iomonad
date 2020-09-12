package streamingIO

import streamingIO.EProcess._
/*
 *多个输入流辅助类T
 */
object Tee{
    final case class T[I,I2]() {
        sealed trait f[X]{def get:Either[I=>X,I2=>X]}
        val L = new f[I]{def get=Left(identity)}
        val R = new f[I2]{def get=Right(identity)}
    }
    def L[I,I2] = T[I,I2]().L
    def R[I,I2] = T[I,I2]().R
    type Tee[I,I2,O] = EProcess[T[I,I2]#f,O]
    /*
     *给Tee输入用的常规函数
     */
    def haltT[I,I2,O]:Tee[I,I2,O] = Halt[T[I,I2]#f,O](End)
    def awaitL[I,I2,O](
            recv:I=>Tee[I,I2,O],
            fallback: =>Tee[I,I2,O] = haltT[I,I2,O]):Tee[I,I2,O] = 
        await[T[I,I2]#f,I,O](L){
            case Left(End) => fallback
            case Left(err) => Halt(err)
            case Right(value) => Try(recv(value)) 
        }
    def awaitR[I,I2,O](
            recv: I2=>Tee[I,I2,O],
            fallback: => Tee[I,I2,O] = haltT[I,I2,O]):Tee[I,I2,O] = 
        await[T[I,I2]#f,I2,O](R) {
            case Left(End) => fallback
            case Left(err) => Halt(err)
            case Right(value) => Try(recv(value))
        }
    def emitT[I,I2,O](h:O,t1:Tee[I,I2,O] = haltT[I,I2,O]):Tee[I,I2,O] = emit(h,t1)
    /*
     *定义一些Tee组合子
     *链状混合（zipping）是Tee特有的一种情况，先从左边读取，然后读取右边，最后递送这一个对值
     *有时可能不希望明确这种顺序，允许驱动器去做出不确定的选择，也允许驱动器能够同时执行这两种选择
     */
    def zipWith[I,I2,O](f:(I,I2)=>O):Tee[I,I2,O] =
        awaitL[I,I2,O](i=>awaitR(i2=>emitT(f(i,i2)))) repeat
    /*
     *在任意一边穷尽时终止
     */
    def zip[I,I2]:Tee[I,I2,(I,I2)] = zipWith((_,_))
    /*
     *类型类为EProcess增加方法——一个函数用Tee来合并两个过程
     */
    implicit class EProcessTee[F[_],O](p:EProcess[F,O]){
        import EProcess1._
        def tee[O2,O3](p2:EProcess[F,O2])(t:Tee[O,O2,O3]):EProcess[F,O3] = 
            t match {
                case Halt(err) => p.kill onComplete(p2.kill) onComplete(Halt(err))
                case Emit(h,t) => Emit(h,(this tee p2)(t))
                case Await(side,recv) => side.get match {
                    case Left(isO) =>  p match{
                        case Halt(e) => p2.kill onComplete Halt(e)
                        case Emit(o,ot) => (ot tee p2)(Try(recv(Right(o))))
                        case Await(reqL,recvL) => await(reqL)(recvL andThen (pp => pp.tee(p2)(t)))
                    }
                    case Right(isO2) => p2 match {
                        case Halt(e) => p.kill onComplete Halt(e)
                        case Emit(o2,ot) => (p tee ot) (Try(recv(Right(o2))))
                        case Await(reqR,recvR) => await(reqR)(recvR andThen (p3 => p.tee(p3)(t)))
                    }
                }
            }
    }
}
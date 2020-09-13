package streamingIO
import EProcess._
import EProcess1._
import Tee._
import Sink._
import java.sql.{PreparedStatement,Connection,ResultSet}
import iomonad.IO
/*
 * 泛化to，使其结果不只是Unit
 */
object Channel {
  implicit class EProcessChannel[F[_],O](p:EProcess[F,O]) {
      def zipWith[O2,O3](p2: EProcess[F,O2])(f: (O,O2) => O3): EProcess[F,O3] =
      (p tee p2)(Tee.zipWith(f))
      def through[O2](p2:EProcess[F,O=>EProcess[F,O2]]):EProcess[F,O2] = 
        join{(p zipWith p2)((o,f)=> f(o))}
  }
  type Channel[F[_],I,O] = EProcess[F,I=>EProcess[F,O]]
  /*
   *使用例子，执行数据库查询，查询的记过是EProcess[IO，Row]
   *这样，所有的程序就可以对查询结果集用上所有的流式转换器
   */
  def query(conn:IO[Connection]):Channel[IO,Connection=>PreparedStatement,Map[String,Any]] = ???
  /*
   *动态资源分配：多个输出流
   */
}

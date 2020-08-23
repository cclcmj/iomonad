package localEffects

trait ST[S,A] {self=>
    protected def run(s:S):(A,S)
    def map[B](f:A=>B): ST[S,B] = new ST[S,B] {
        def run(s:S) = {
            val (a,s1) = self.run(s)
            (f(a),s1)
        }
    }
    def flatMap[B](f:A=>ST[S,B]):ST[S,B] = new ST[S,B]{
        def run(s: S) = {
            val (a,s1) = self.run(s)
            f(a).run(s1)
        }
    }
}
object ST {
    def apply[S,A](a: =>A) = {
        //缓存a以便run多次访问
        lazy val memo = a
        new ST[S,A] {
            def run(s: S): (A, S) = (memo,s)
        }
    }
    //利用RunnableST来运行ST
    def runST[A](st:RunnableST[A]):A = 
        st.apply[Unit].run(())._1
    //例子
    val p = new RunnableST[(Int,Int)] {
        def apply[S]= for{
            r1 <- STRef(1)
            r2 <- STRef(2)
            x <- r1.read
            y <- r2.read
            _ <- r1.write(y+1)
            _ <- r2.write(x+1)
            a <- r1.read
            b <- r2.read
        } yield (a,b)
    }
    val r = runST(p)
}
//一种可变引用的代数表达,指定初始状态
sealed trait STRef[S,A] {
    protected var cell:A
    def read:ST[S,A] = ST(cell)
    def write(a:A):ST[S,Unit] = new ST[S,Unit]{
        def run(s: S): (Unit, S) = {
            cell = a
            ((),s)
        }
    }
}
object STRef {
    def apply[S,A](a:A):ST[S,STRef[S,A]] = ST(new STRef[S,A] {
        var cell = a
    })
}
//执行修改状态的行为，运行ST
trait RunnableST[A] {
    def apply[S]:ST[S,A]
}

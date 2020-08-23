package iomonad

trait Files[A]
trait HandleR
trait HandleW
case class OpenRead(file:String) extends Files[HandleR]
case class OpenWrite(file:String) extends Files[HandleW]
case class FReadLine(h:HandleR) extends Files[Option[String]]
case class FWriteLine(h:HandleW,line:String) extends Files[Unit]
object Files{
    def loop(f:HandleR,c:HandleW):Free[Files,Unit] = for{
        line <- FSuspend{ FReadLine(f) }
        _ <- line match{
            case None => FSuspend(FWriteLine(c,""))
            case Some(s)=> FSuspend{
                                FWriteLine(c,TemperatureConverter.fahrenheitToCelsius(s.toDouble).toString())
                            } flatMap (_=> loop(f,c))
        }
    } yield ()
}

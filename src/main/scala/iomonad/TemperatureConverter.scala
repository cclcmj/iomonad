package iomonad

import scala.io.StdIn

//13.2 温度转换器
object TemperatureConverter {
  def fahrenheitToCelsius(f:Double):Double = (f-32)*5.0/9.0
  def readLine:IO[String] = IO {StdIn.readLine()}
  def printLine(msg:String):IO[Unit] = IO {println(msg)}
  def converter:IO[Unit] =  for {
      _ <- printLine("Enter a temperature in degrees Fahrenheit: ")
      d <- readLine.map(_.toDouble)
      _ <- printLine(fahrenheitToCelsius(d).toString)
  } yield ()
}

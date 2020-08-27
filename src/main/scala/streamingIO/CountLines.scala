package streamingIO

import iomonad.IO

object CountLines {
  //统计文件的行数是否超过40000
  def linesGt40k(filename:String):IO[Boolean] = IO{
     val src = io.Source.fromFile(filename)
     try {
         var count = 0
         val lines : Iterator[String] = src.getLines()
         while(count<=40000 && lines.hasNext){
             lines.next()
             count+=1
         }
         count>40000
     }
     finally src.close()
  }
}

import java.io.{File, FileNotFoundException}

import scala.io.Source
  
object WordCount extends App {

  val path = "C:\\Users\\zhch\\Downloads\\detail-productdb-service.2017-11-29.log"
  val file = new File(path)
  val files = file.listFiles().filter(_.isFile)
  val mapData = scala.collection.mutable.Map[String, Int]()

  def readFile(item: File): Unit = {
    try {
      Source.fromFile(item).getLines().flatMap(_.split("trans-pool-1-thread-\\d{1,2}")).toList.map(x => {
        mapData.get(x) match {
          case Some(b) => mapData += (x -> b.+(1))
          case None => mapData += (x -> 1)
        }
      })
    } catch {
      case e1: FileNotFoundException => println("FileNotFoundException")
      case e2: RuntimeException => println("RuntimeException")
      case e3: Exception => println("Exception")
    }
  }

  files.map(readFile)
  println(mapData)
}
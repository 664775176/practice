import scala.util.matching.Regex

object Test {
   def main(args: Array[String]) {
      val pattern = new Regex("trans-pool-1-thread-\\d{1,2}")
      val str = "11-29 23:57:00 030 trans-pool-1-thread-3 DEBUG - Initiating transaction commit"
      
      println((pattern findAllIn str).mkString(","))
   }
}
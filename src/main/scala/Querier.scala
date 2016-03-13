import java.io._
import scala.util._

case class Querier(directory: File) {
  def run(): Unit = {
    val errorCount =
      (for {
        i <- 'a' to 'z'
        j <- 'a' to 'z'
        k <- 'a' to 'z'
        query = s"$i$j$k"
      } yield downloadForQuery(query)).sum

    println(s"Error count: $errorCount")
  }

  private def downloadForQuery(query: String): Int =
    SwissCommercialRegister.reportLinks(query) match {
      case Success(reportLinks) =>
        (for (reportLink <- reportLinks) yield downloadForLink(reportLink)).sum
      case Failure(throwable) =>
        println(s"Could not retrieve results for '$query': ${throwable.getMessage}")
        1
    }
}

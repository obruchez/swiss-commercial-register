import java.io._
import scala.util._

case class Querier(directory: File) {
  def run(): Unit = {
    val errorCount =
      (for {
        i <- 'a' to 'a' //'z'
        j <- 'a' to 'a' //'z'
        k <- 'a' to 'a' //'z'
        query = s"$i$j$k"
      } yield downloadForQuery(query)).sum

    println(s"Error count: $errorCount")
  }

  private def downloadForQuery(query: String): Int =
    SwissCommercialRegister.reportLinks(query) match {
      case Success(reportLinks) =>
        for (reportLink <- reportLinks) yield {
          Dispatcher.dispatcher ! Dispatch(directory, reportLink)
          // @todo accumulate and sequence futures?
        }

        // @todo
        0
      case Failure(throwable) =>
        println(s"Could not retrieve results for '$query': ${throwable.getMessage}")
        1
    }
}

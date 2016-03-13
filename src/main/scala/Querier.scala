import akka.actor._
import akka.util.Timeout
import java.io._
import scala.concurrent.duration._
import scala.language.postfixOps

case class Querier(directory: File) {
  def run(): Unit = {
    for {
      i <- 'a' to 'a' //'z'
      j <- 'a' to 'a' //'z'
      k <- 'a' to 'a' //'z'
      query = s"$i$j$k"
    } {
      Dispatcher.dispatcher ! Dispatcher.DownloadQuery(directory, query)
    }

    // @todo wait for downloads to be finished
    implicit val timeout = Timeout(72 hours)
    //Dispatcher.dispatcher ? Dispatcher.???
  }
}

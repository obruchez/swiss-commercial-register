import akka.actor.{Props, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import java.io._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps

case class Searcher(directory: File) {
  implicit val timeout = Timeout(72 hours)

  def run(): Unit = {
    val resultFuture = Future.sequence {
      for {
        i <- 'a' to 'a' //'z' @todo
        j <- 'a' to 'a' //'z' @todo
        k <- 'a' to 'a' //'z' @todo
        searchQuery = s"$i$j$k"
      } yield {
        val askFuture = dispatcher ? Dispatcher.DownloadSearch(searchQuery, retryCount = Dispatcher.DefaultRetryCount)
        askFuture.mapTo[Dispatcher.SearchDownloadResult].map(sdr => sdr.query -> sdr.result)
      }
    }

    val results = Await.result(resultFuture, timeout.duration)

    val successCount = results.count(_._2.isSuccess)
    val failureCount = results.count(_._2.isFailure)

    println(s"Success count: $successCount")
    println(s"Failure count: $failureCount")
  }

  private val system = ActorSystem("System")
  private val dispatcher = system.actorOf(Props(new Dispatcher(directory)), name = "master")

  def stop(): Unit = {
    system.terminate()
  }
}

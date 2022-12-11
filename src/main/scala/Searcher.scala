import akka.actor.{Props, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import java.io._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps

case class Searcher(directory: File) {
  implicit val timeout = Timeout(1000 hours)

  def run(): Unit = {
    val searchQueries =
      for {
        i <- 'a' to 'z'
        j <- 'a' to 'z'
        k <- 'a' to 'z'
      } yield s"$i$j$k"

    // Don't send search queries in parallel
    val results =
      for {
        (searchQuery, index) <- searchQueries.zipWithIndex
      } yield {
        println(
          f"Progress: $index/${searchQueries.size} (${index.toDouble / searchQueries.size.toDouble * 100.0}%.1f%%)"
        )

        val askFuture = dispatcher ? Dispatcher.DownloadSearch(
          searchQuery,
          retryCount = Dispatcher.DefaultRetryCount
        )
        val resultFuture = askFuture
          .mapTo[Dispatcher.SearchDownloadResult]
          .map(sdr => sdr.query -> sdr.result)
        Await.result(resultFuture, timeout.duration)
      }

    val successCount = results.count(_._2.isSuccess)
    val failureCount = results.count(_._2.isFailure)

    println(s"Success count: $successCount")
    println(s"Failure count: $failureCount")
  }

  private val system = ActorSystem("System")
  private val dispatcher =
    system.actorOf(Props(new Dispatcher(directory, timeout)), name = "master")

  def stop(): Unit = {
    system.terminate()
  }
}

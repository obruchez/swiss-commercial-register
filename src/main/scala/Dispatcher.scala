import akka.actor._
import akka.routing.ConsistentHashingPool
import akka.util.Timeout
import java.io.File
import scala.language.postfixOps
import scala.util._

case class Dispatcher(directory: File, implicit val timeout: Timeout) extends Actor {
  import context._

  def receive: Receive = {
    case Dispatcher.DownloadSearch(query, retryCount) =>
      createSearchState(sender(), query, retryCount)

    case Downloader.SearchDownloadResult(query, result) =>
      updateSearchState(query, result)

    case Downloader.LinkDownloadResult(query, link, result) =>
      updateSearchState(query, link, result)
  }

  private case class LinkToDownload(link: SwissCommercialRegister.Link, remainingRetryCount: Int)

  private trait SearchState
  private case class Searching(remainingRetryCount: Int) extends SearchState
  private case class Downloading(linksToDownload: Seq[LinkToDownload]) extends SearchState {
    def withLinkRemoved(link: SwissCommercialRegister.Link): Downloading =
      copy(linksToDownload = this.linksToDownload.filterNot(_.link == link))

    def withOneRetryLess(link: SwissCommercialRegister.Link): Downloading =
      copy(linksToDownload = this.linksToDownload flatMap { linkToDownload =>
        if (linkToDownload.link == link) {
          if (linkToDownload.remainingRetryCount <= 0) {
            None
          } else {
            Some(linkToDownload.copy(remainingRetryCount = linkToDownload.remainingRetryCount - 1))
          }
        } else {
          Some(linkToDownload)
        }
      })

    def remainingRetryCount(link: SwissCommercialRegister.Link): Option[Int] =
      linksToDownload.find(_.link == link).map(_.remainingRetryCount)
  }

  private case class Search(sender: ActorRef, query: String, retryCount: Int, state: SearchState)

  private val searchesByQuery = collection.mutable.Map[String, Search]()

  private def createSearchState(sender: ActorRef, query: String, retryCount: Int): Unit = {
    searchesByQuery(query) = Search(sender, query, retryCount, Searching(retryCount))

    downloader ! Downloader.DownloadSearch(query)
  }

  private def updateSearchState(
      query: String,
      result: Try[Seq[SwissCommercialRegister.Link]]
  ): Unit =
    searchesByQuery.get(query) foreach { search =>
      // Ignore message if query not found

      result match {
        case Success(links) =>
          if (links.nonEmpty) {
            val linksToDownload =
              links.map(LinkToDownload(_, remainingRetryCount = search.retryCount))
            searchesByQuery(query) = search.copy(state = Downloading(linksToDownload))

            links.foreach(link => downloader ! Downloader.DownloadLink(query, link))
          } else {
            search.sender ! Dispatcher.SearchDownloadResult(query, Success(()))
          }
        case Failure(throwable) =>
          search.state match {
            case Searching(remainingRetryCount) =>
              if (remainingRetryCount <= 0) {
                // No retry left => failure
                searchesByQuery.remove(query)

                search.sender ! Dispatcher.SearchDownloadResult(query, Failure(throwable))
              } else {
                // Retry to download search page(s)
                searchesByQuery(query) = search.copy(state = Searching(remainingRetryCount - 1))

                downloader ! Downloader.DownloadSearch(query)
              }
            case _ =>
              // Unexpected state
              searchesByQuery.remove(query)

              val exception =
                new Exception(s"Unexpected state while searching for '$query'")
              search.sender ! Dispatcher.SearchDownloadResult(query, Failure(exception))
          }
      }
    }

  private def updateSearchState(
      query: String,
      link: SwissCommercialRegister.Link,
      result: Try[Unit]
  ): Unit =
    searchesByQuery.get(query) foreach { search =>
      // Ignore message if query not found

      search.state match {
        case downloading @ Downloading(linksToDownload) =>
          val newDownloadingState =
            result match {
              case Success(_) =>
                downloading.withLinkRemoved(link)
              case Failure(throwable) =>
                val withOneRetryLess = downloading.withOneRetryLess(link)

                withOneRetryLess.remainingRetryCount(link) match {
                  case Some(_) =>
                    // Retry to download link
                    downloader ! Downloader.DownloadLink(query, link)
                  case None =>
                }

                withOneRetryLess
            }

          if (newDownloadingState.linksToDownload.isEmpty) {
            // No more link to download
            searchesByQuery.remove(query)

            search.sender ! Dispatcher.SearchDownloadResult(query, result)
          } else {
            // Wait for links to download
            searchesByQuery(query) = search.copy(state = newDownloadingState)
          }
        case _ =>
          // Unexpected state
          searchesByQuery.remove(query)

          val exception =
            new Exception(s"Unexpected state while searching for '$query'")
          search.sender ! Dispatcher.SearchDownloadResult(query, Failure(exception))
      }
    }

  // Make sure a given URL will always be mapped to the same actor
  private val downloaderHashMapping: PartialFunction[Any, Any] = {
    case Downloader.DownloadSearch(query) => query
    case Downloader.DownloadLink(_, link) => link.url.toString
  }

  private val DownloaderPoolSize = 5

  private val downloader = system.actorOf(
    ConsistentHashingPool(DownloaderPoolSize, hashMapping = downloaderHashMapping)
      .props(Props(new Downloader(directory, self))),
    "downloader-pool"
  )
}

object Dispatcher {
  val DefaultRetryCount = 10

  sealed trait Message
  case class DownloadSearch(query: String, retryCount: Int) extends Message
  case class DownloadLink(ink: SwissCommercialRegister.Link, retryCount: Int) extends Message

  sealed trait Response
  case class SearchDownloadResult(query: String, result: Try[Unit]) extends Response
}

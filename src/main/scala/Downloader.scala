import akka.actor.{Actor, ActorRef}
import java.io.{File, PrintWriter}
import scala.util._

class Downloader(dispatcher: ActorRef) extends Actor {
  def receive = {
    case Downloader.DownloadQuery(directory, query) =>
      SwissCommercialRegister.reportLinks(query) match {
        case Success(reportLinks) =>
          for (reportLink <- reportLinks) {
            Dispatcher.dispatcher ! Dispatcher.DownloadLink(directory, reportLink)
          }
          // @todo accumulate results and then send back result to sender
        case Failure(throwable) =>
          sender() ! Downloader.QueryDownloadResult(query: String, Failure(throwable))
      }

    case Downloader.DownloadLink(directory, link) =>
      val result = Downloader.downloadForLink(directory, link)
      sender() ! Downloader.LinkDownloadResult(link, result)
  }

  //private val queryResults: collection.mutable.Map[String, ]
}

object Downloader {
  sealed trait Message
  case class DownloadQuery(directory: File, query: String) extends Message
  case class DownloadLink(directory: File, link: SwissCommercialRegister.Link) extends Message

  sealed trait Response
  case class QueryDownloadResult(query: String, result: Try[Unit])
  case class LinkDownloadResult(link: SwissCommercialRegister.Link, result: Try[Unit])

  private def downloadForLink(directory: File, link: SwissCommercialRegister.Link): Try[Unit] = {
    val file = new File(directory, s"${link.description}.html")

    if (file.exists()) {
      Success(())
    } else {
      for {
        content <- link.content()
        _ <- saveToFile(content, file)
      } yield ()
    }
  }

  private def saveToFile(string: String, file: File): Try[Unit] = Try {
    val writer = new PrintWriter(file)

    try {
      writer.write(string)
    } finally {
      writer.close()
    }
  }
}

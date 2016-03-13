import akka.actor.{Actor, ActorRef}
import java.io.{File, PrintWriter}
import scala.util._

class Downloader(dispatcher: ActorRef) extends Actor {
  def receive = {
    case Downloader.DownloadForQuery(directory, query) =>
      SwissCommercialRegister.reportLinks(query) match {
        case Success(reportLinks) =>
          for (reportLink <- reportLinks) {
            Dispatcher.dispatcher ! Dispatcher.DownloadLink(directory, reportLink)
          }
        case Failure(throwable) =>
      }
      // @todo send result to dispatcher
    case Downloader.DownloadLink(directory, link) =>
      Downloader.downloadForLink(directory, link) match {
        case Success(_) =>
        case Failure(throable) =>
      }
      // @todo send result to dispatcher
  }

  //private val queryResults: collection.mutable.Map[String, ]
}

object Downloader {
  sealed trait Message
  case class DownloadForQuery(directory: File, query: String) extends Message
  case class DownloadLink(directory: File, link: SwissCommercialRegister.Link) extends Message

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

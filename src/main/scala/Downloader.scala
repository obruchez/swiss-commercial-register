import akka.actor.{Actor, ActorRef}
import java.io.{File, PrintWriter}
import scala.util._

class Downloader(directory: File, dispatcher: ActorRef) extends Actor {
  def receive = {
    case Downloader.DownloadSearch(query) =>
      val result = SwissCommercialRegister.reportLinks(query)
      sender() ! Downloader.SearchDownloadResult(query, result)

    case Downloader.DownloadLink(query, link) =>
      val result = Downloader.downloadForLink(directory, link)
      sender() ! Downloader.LinkDownloadResult(query, link, result)
  }
}

object Downloader {
  sealed trait Message
  case class DownloadSearch(query: String) extends Message
  case class DownloadLink(query: String, link: SwissCommercialRegister.Link) extends Message

  sealed trait Response
  case class SearchDownloadResult(query: String, result: Try[Seq[SwissCommercialRegister.Link]])
  case class LinkDownloadResult(query: String,
                                link: SwissCommercialRegister.Link,
                                result: Try[Unit])

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

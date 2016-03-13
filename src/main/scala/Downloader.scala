import akka.actor.{Actor, ActorRef}
import java.io.{File, PrintWriter}
import scala.util._

sealed trait DownloaderMessage
case class Download(directory: File, link: SwissCommercialRegister.Link) extends DownloaderMessage

class Downloader(dispatcher: ActorRef) extends Actor {
  def receive = {
    case Download(directory, link) =>
      Downloader.downloadForLink(directory, link)
      // @todo send result to dispatcher
  }
}

object Downloader {
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

import java.io._
import scala.util._

case class Downloader(directory: File) {
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

  private def downloadForLink(link: SwissCommercialRegister.Link): Int = {
    val file = new File(directory, s"${link.description}.html")

    if (file.exists()) {
      0
    } else {
      val result =
        for {
          content <- SwissCommercialRegister.pageContent(link.url)
          _ <- saveToFile(content, file)
        } yield ()

      result match {
        case Success(_) =>
          0
        case Failure(throwable) =>
          println(s"Could not save file '${file.getAbsolutePath}' for URL '${link.url}': ${throwable.getMessage}")
          1
      }
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

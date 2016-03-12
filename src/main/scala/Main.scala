import java.net.URL
import scala.io.{Codec, Source}
import scala.util._

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")

    val rc = resultCount("aaf")
    println(s"rc = $rc")
  }

  def test(): Unit = {
    val url = new URL("http://www.zefix.ch/WebServices/Zefix/Zefix.asmx/SearchFirm?name=aaf%20&suche_nach=aktuell&rf=&sitz=&sitzgem=&id=&language=4&phonetisch=no&posMin=-1")
    val string = Source.fromURL(url)(Codec("UTF-8")).mkString
    println(string)
  }

  def resultCount(query: String): Try[Int] = {
    val CountPattern = """<b>\((\d+) results on the""".r

    def pageContent(): Try[String] = Try {
      val url = new URL(s"http://www.zefix.ch/WebServices/Zefix/Zefix.asmx/SearchFirm?name=$query&suche_nach=aktuell&rf=&sitz=&sitzgem=&id=&language=4&phonetisch=no&posMin=-1")
      Source.fromURL(url)(Codec("UTF-8")).mkString
    }

    def countFromContent(content: String): Try[Int] =
      CountPattern.findFirstMatchIn(content) match {
        case Some(m) => Try(m.group(1).toInt)
        case None => Failure(new Exception("No result count found in page"))
      }

    for {
      content <- pageContent()
      count <- countFromContent(content)
    } yield count
  }
}

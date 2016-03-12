import java.net.URL

import scala.io.{Codec, Source}
import scala.util._

object SwissCommercialRegister {
  case class Link(url: URL, description: String)

  private val PositionsPerPage = 1500

  def reportLinks(query: String): Try[Seq[Link]] = {
    @annotation.tailrec
    def reportLinks(query: String, position: Int, acc: Seq[Link] = Seq()): Try[Seq[Link]] =
      pageContent(query, position).flatMap(reportLinksFromContent) match {
        case Success(links) if links.isEmpty =>
          Success(acc)
        case Success(links) =>
          reportLinks(query, position = position + PositionsPerPage, acc = acc ++ links)
        case failure =>
          failure
      }

    reportLinks(query, position = 0)
  }

  private val CompanyIdPrefixes = Set("CH-", "CHE-")

  private def reportLinksFromContent(content: String): Try[Seq[Link]] =
    allLinksFromContent(content) map { links =>
      links.filter(link => CompanyIdPrefixes.exists(link.description.startsWith))
    }

  private val LinkPattern = """<a[^>]+href="([^"]*)"[^>]*>([^<]*)<\/a>""".r

  private def allLinksFromContent(content: String): Try[Seq[Link]] = Try {
    for {
      m <- LinkPattern.findAllMatchIn(content).toList
      urlString = m.group(1)
      description = m.group(2)
    } yield Link(url = new URL(if (urlString.startsWith("http://")) urlString else s"$BaseUrl$urlString"), description)
  }

  private def resultCount(query: String): Try[Int] =
    for {
      content <- pageContent(query, position = -1)
      count <- countFromContent(content)
    } yield count

  private val CountPattern = """<b>\((\d+) results on the""".r

  private def countFromContent(content: String): Try[Int] =
    CountPattern.findFirstMatchIn(content) match {
      case Some(m) => Try(m.group(1).toInt)
      case None => Failure(new Exception("No result count found in page"))
    }

  def pageContent(url: URL): Try[String] =
    Try(Source.fromURL(url)(Codec("ISO-8859-1")).mkString)

  private def pageContent(query: String, position: Int): Try[String] =
    pageContent(url(query, position))

  private val BaseUrl = "http://www.zefix.ch"

  private def url(query: String, position: Int): URL =
    new URL(s"$BaseUrl/WebServices/Zefix/Zefix.asmx/SearchFirm?name=$query&suche_nach=-&rf=&sitz=&sitzgem=&id=&language=4&phonetisch=no&posMin=$position")
}

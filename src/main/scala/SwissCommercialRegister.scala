import java.net.URL

import scala.io.{Codec, Source}
import scala.util._

object SwissCommercialRegister {
  case class Link(url: URL, description: String) {
    def content(): Try[String] =
      pageContent(url) flatMap { content =>
        documentMovedContent(content) orElse frameContent(content) orElse Success(content)
      } flatMap { content =>
        // Check for empty content
        if (
          content.length < 1000 || content.toLowerCase.contains(
            "an error has occurred. please try again later."
          )
        ) {
          Failure(new Exception("No content"))
        } else {
          Success(content)
        }
      }

    private def documentMovedContent(content: String): Try[String] =
      if (
        content.toLowerCase.contains("document moved") || content.toLowerCase
          .contains("document has moved")
      ) {
        allLinksFromContent(baseUrl = Link.baseUrl(url).toString, content)
          .flatMap(contentOfFirstLinkWithDescriptionInUrl)
      } else {
        Failure(new NoSuchElementException)
      }

    private def frameContent(content: String): Try[String] =
      allFrameLinksFromContent(baseUrl = Link.baseUrl(url).toString, content)
        .flatMap(contentOfFirstLinkWithDescriptionInUrl)

    private def contentOfFirstLinkWithDescriptionInUrl(links: Seq[Link]): Try[String] =
      contentOfFirstLinkWithStringInUrl(links, string = description) orElse
        contentOfFirstLinkWithStringInUrl(links, string = "companyOfrcId13")

    private def contentOfFirstLinkWithStringInUrl(links: Seq[Link], string: String): Try[String] =
      links.find(_.url.toString.contains(string)) match {
        case Some(link) =>
          pageContent(link.url)
        case None =>
          Failure(new NoSuchElementException)
      }
  }

  object Link {
    def baseUrl(url: URL): URL = {
      val BaseUrlPattern = """(https?://[^/]+)""".r
      BaseUrlPattern.findFirstIn(url.toString).map(new URL(_)).getOrElse(url)
    }

    def cleanUrl(url: String): String =
      url.replaceAll("&amp;", "&").replaceAll("&#59;jsessionid=[a-z0-9]+", "")
  }

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
    allLinksFromContent(baseUrl = BaseQueryUrl, content) map { links =>
      links.filter(link => CompanyIdPrefixes.exists(link.description.startsWith))
    }

  private val LinkPattern = """<a[^>]+href="([^"]*)"[^>]*>([^<]*)<\/a>""".r

  private def allLinksFromContent(baseUrl: String, content: String): Try[Seq[Link]] = Try {
    for {
      m <- LinkPattern.findAllMatchIn(content).toList
      urlString = Link.cleanUrl(m.group(1))
      description = m.group(2)
    } yield Link(
      url = new URL(if (urlString.startsWith("http")) urlString else s"$baseUrl$urlString"),
      description
    )
  }

  private val FramePattern = """<frame[^>]+src="([^"]*)"""".r

  private def allFrameLinksFromContent(baseUrl: String, content: String): Try[Seq[Link]] = Try {
    for {
      m <- FramePattern.findAllMatchIn(content).toList
      urlString = m.group(1).replaceAll("&amp;", "&")
    } yield Link(
      url = new URL(if (urlString.startsWith("http")) urlString else s"$baseUrl$urlString"),
      description = ""
    )
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
      case None    => Failure(new Exception("No result count found in page"))
    }

  private def pageContent(url: URL, encoding: String): Try[String] = Try {
    import scalaj.http._

    val response: HttpResponse[String] = Http(url.toString).asString

    response.header("Location") match {
      case Some(urlString) =>
        pageContent(new URL(urlString), encoding).get
      case None =>
        response.body
    }

    // Try(Source.fromURL(url)(Codec(encoding)).mkString)
  }

  private def pageContent(url: URL): Try[String] =
    pageContent(url: URL, "UTF-8") orElse pageContent(url: URL, "ISO-8859-1")

  private def pageContent(query: String, position: Int): Try[String] =
    pageContent(queryUrl(query, position))

  private val BaseQueryUrl = "http://www.zefix.ch"

  private def queryUrl(query: String, position: Int): URL =
    new URL(
      s"$BaseQueryUrl/WebServices/Zefix/Zefix.asmx/SearchFirm?" + s"name=${query
        .replaceAll(" ", "%20")}&" + s"suche_nach=-&rf=&sitz=&sitzgem=&id=&language=4&phonetisch=no&posMin=$position"
    )
}

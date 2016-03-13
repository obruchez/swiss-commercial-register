import akka.actor._
import akka.routing.ConsistentHashingPool
import java.io.File

class Dispatcher extends Actor {
  import context._

  def receive = {
    case Dispatcher.DownloadQuery(directory, query) =>
      Dispatcher.downloader ! Downloader.DownloadQuery(directory, query)
      // @todo send back the result
    case Dispatcher.DownloadLink(directory, link) =>
      Dispatcher.downloader ! Downloader.DownloadLink(directory, link)
      // @todo send back the result

      /*if (reschedule) {
        system.scheduler.scheduleOnce(Master.CheckPeriod, self, CheckCache)
      }*/
  }
}

object Dispatcher {
  sealed trait Message
  case class DownloadQuery(directory: File, query: String) extends Message
  case class DownloadLink(directory: File, link: SwissCommercialRegister.Link) extends Message

  lazy val system = ActorSystem("System")
  lazy val dispatcher = system.actorOf(Props[Dispatcher], name = "master")

  // Make sure a given URL will always be mapped to the same actor
  private val downloaderHashMapping: PartialFunction[Any, Any] = {
    case Downloader.DownloadQuery(_, query) => query
    case Downloader.DownloadLink(_, link) => link.url.toString
  }

  lazy val downloader = system.actorOf(
    ConsistentHashingPool(5, hashMapping = downloaderHashMapping).props(Props(new Downloader(dispatcher))),
    "downloader-pool")

  def stop(): Unit = {
    system.terminate()
    // @todo wait for termination?
  }
}

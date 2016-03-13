import java.io.{PrintWriter, File}

import akka.actor._
import akka.routing.RoundRobinPool
//import org.joda.time.DateTime
//import org.joda.time.DateTime
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util._

import akka.routing.ConsistentHashingPool
import akka.routing.ConsistentHashingRouter.ConsistentHashable

sealed trait DispatcherMessage
case class Dispatch(directory: File, link: SwissCommercialRegister.Link) extends DispatcherMessage

class Dispatcher extends Actor {
  import context._

  def receive = {
    case Dispatch(directory, link) =>
      Dispatcher.downloader ! Download(directory, link)
      // @todo send back the result

      // @todo
      /*if (reschedule) {
        system.scheduler.scheduleOnce(Master.CheckPeriod, self, CheckCache)
      }*/
  }
}

object Dispatcher {
  lazy val system = ActorSystem("System")
  lazy val dispatcher = system.actorOf(Props[Dispatcher], name = "master")

  // Make sure a given URL will always be mapped to the same actor
  private val downloaderHashMapping: PartialFunction[Any, Any] = {
    case Download(directory, link) =>
      link.url.toString
  }

  lazy val downloader = system.actorOf(
    ConsistentHashingPool(5, hashMapping = downloaderHashMapping).props(Props(new Downloader(dispatcher))),
    "downloader-pool")

  /*def start(): Unit = {
    master ! CheckCache(force = false, reschedule = true)
  }*/

  def stop(): Unit = {
    system.terminate()
    // @todo wait for termination?
  }

  /*def forceFetch(): Unit = {
    master ! CheckCache(force = true, reschedule = false)
  }*/
}

/*
dispatch to different downloaders (use hash to avoid collisions)
 */

import java.io.{PrintWriter, File}

import akka.actor._
import akka.routing.RoundRobinPool
//import org.joda.time.DateTime
//import org.joda.time.DateTime
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util._

sealed trait DispatcherMessage
case class Download(directory: File, link: SwissCommercialRegister.Link) extends DispatcherMessage

class Dispatcher extends Actor {
  import context._

  def receive = {
    case Download(directory, link) =>
      // @todo dispatch
      Dispatcher.downloadForLink(directory, link)

      // @todo
      /*if (reschedule) {
        system.scheduler.scheduleOnce(Master.CheckPeriod, self, CheckCache)
      }*/
  }
}

object Dispatcher {
  lazy val system = ActorSystem("System")
  lazy val master = system.actorOf(Props[Dispatcher], name = "master")
  //lazy val cache = system.actorOf(Props[Cache], name = "cache")
  //lazy val fetcherRouter = system.actorOf(RoundRobinPool(10).props(Props(new Fetcher(cache))), "fetcher-pool")

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

/*
dispatch to different downloaders (use hash to avoid collisions)
 */

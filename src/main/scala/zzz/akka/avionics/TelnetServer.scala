package zzz.akka.avionics

import akka.actor._
import akka.util.{Timeout, ByteString}
import scala.concurrent.duration._
import scala.util.{Success, Failure}

class TelnetServer(plane: ActorRef) extends Actor with ActorLogging {
  import TelnetServer._

  var subservers = Map.empty[IO.Handle, ActorRef]

  val serverSocket = IOManager(context.system).listen("0.0.0.0", 31733)

  def receive = {
    case IO.Listening(server, address) =>
      log.info("Telnet Server listening on port {}", address)
    case IO.NewClient(server) =>
      log.info("New incoming client connection on server")
      val socket = server.accept()
      socket.write(ByteString(welcome))
      subservers += (socket -> context.actorOf(Props(new SubServer(socket, plane))))
    case IO.Read(socket,bytes) =>
      val cmd = ascii(bytes)
      subservers(socket) ! NewMessage(cmd)
    case IO.Closed(socket,cause) =>
      context.stop(subservers(socket))
      subservers -= socket

  }

}

object TelnetServer {
  implicit val askTimeout = Timeout(1.second)

  val welcome =
  """
    |Welcome to the Airplane!
    |----------------
    |
    |Valid commands are: 'heading' and 'altitude'
    |
    |>""".stripMargin

  def ascii(bytes: ByteString): String = bytes.decodeString("UTF-8").trim

  case class NewMessage(msg: String)

  class SubServer(socket: IO.SocketHandle, plane: ActorRef) extends Actor {
    import HeadingIndicator._
    import Altimeter._
    import akka.pattern.ask

    implicit val ec = context.dispatcher

    def headStr(head:Float): ByteString = ByteString(f"current heading is: $head%3.2f degrees\n\n> ")
    def altStr(alt:Double): ByteString = ByteString(f"current altitude is: $alt%5.2f feet\n\n> ")
    def unknown(str: String): ByteString = ByteString(f"current $str is: unknown\n\n> ")

    def handleHeading() = {
      (plane ? GetCurrentHeading).mapTo[CurrentHeading].onComplete {
        case Success(CurrentHeading(h)) => socket.write(headStr(h))
        case Failure(_) => socket.write(unknown("heading"))
      }
    }

    def handleAltitude() = {
      (plane ? GetCurrentAltitude).mapTo[CurrentAltitude].onComplete {
        case Success(CurrentAltitude(a)) => socket.write(altStr(a))
        case Failure(_) => socket.write(unknown("altitude"))
      }
    }

    def receive = {
      case NewMessage(msg) =>
        msg match {
          case "heading" => handleHeading()
          case "altitude" => handleAltitude()
          case m => socket.write(ByteString("What?\n\n"))
        }
    }
  }
}

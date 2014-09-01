package zzz.akka.avionics

import akka.actor._
import akka.testkit.{TestActorRef, ImplicitSender, TestKit}
import akka.util.ByteString
import org.scalatest.{Matchers, WordSpecLike}


class PlaneForTelnet extends Actor {
  import HeadingIndicator._
  import Altimeter._

  def receive = {
    case GetCurrentAltitude => sender ! CurrentAltitude(35000f)
    case GetCurrentHeading => sender ! CurrentHeading(240f)
  }
}

class TelnetServerSpec extends TestKit(ActorSystem("TelnetServerSpec")) with ImplicitSender with Matchers with WordSpecLike {

  "TelnetServer" should {
    "work" in {
      val p = TestActorRef[PlaneForTelnet]
      val s = TestActorRef[TelnetServer](Props(new TelnetServer((p))))
      val socket = IOManager(system).connect("localhost", 31733)
      expectMsgType[IO.Connected]
      expectMsgType[IO.Read]
      socket.write(ByteString("heading"))
      expectMsgPF() {
        case IO.Read(_, bytes) =>
          val result = TelnetServer.ascii(bytes)
          result should include ("240.00 degrees")
      }

      socket.write(ByteString("altitude"))
      expectMsgPF() {
        case IO.Read(_, bytes) =>
          val result = TelnetServer.ascii(bytes)
          result should include ("35000.00 feet")
      }

      socket.close()
    }
  }
}

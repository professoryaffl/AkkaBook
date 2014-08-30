package zzz.akka.avionics

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import com.typesafe.config.ConfigFactory
import org.scalatest.WordSpecLike


class TestFlightAttendant extends FlightAttendant with AttendantResponsiveness {
  override val maxResponseTimeMS: Int = 1
}

class FlightAttendantSpec extends TestKit(ActorSystem("FlightAttendantSpec", ConfigFactory.parseString("akka.scheduler.tick-duration = 1ms"))) with WordSpecLike with ImplicitSender {

  import FlightAttendant._

  "FlightAttendant" should {
    "Server a drink within time" in {
      val attendant = TestActorRef[TestFlightAttendant]
      attendant ! GetDrink("g&t")
      expectMsg(Drink("g&t"))
    }
  }
}

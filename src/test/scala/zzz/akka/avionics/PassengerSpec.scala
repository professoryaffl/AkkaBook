package zzz.akka.avionics

import akka.actor.{Actor, ActorRef, Props, ActorSystem}
import akka.testkit.{TestActorRef, ImplicitSender, TestKit}
import com.typesafe.config.ConfigFactory
import org.scalatest.{Matchers, WordSpecLike}
import zzz.akka.avionics.Passenger.FastenSeatbelts
import zzz.akka.avionics.PassengerSupervisor.{GetPassengerBroadcaster, PassengerBroadcaster}

import scala.concurrent.duration._

trait TestDrinkRequestProbability extends DrinkRequestProbability {
  override val askThreshold: Float = 0f
  override val requestMin: FiniteDuration = 0.millis
  override val requestUpper: FiniteDuration = 2.millis
}

class PassengerSpec extends TestKit(ActorSystem("PassengerSpec")) with ImplicitSender with WordSpecLike with Matchers {
  import akka.event.Logging.Info
  import akka.testkit.TestProbe

  var seatNumber = 9
  def newPassenger(): ActorRef = {
    seatNumber += 1
    system.actorOf(Props(new Passenger(testActor) with TestDrinkRequestProbability), s"Foo_Bar-$seatNumber-B")
  }

  "Passengers" should {
    "fasten sesatbelts when asked" in {
      val a = newPassenger()
      val p = TestProbe()
      system.eventStream.subscribe(p.ref, classOf[Info])
      a ! FastenSeatbelts
      p.expectMsgPF() {
        case Info(_,_,m) =>
          m.toString should include ("fastening seatbelt")
      }
    }
  }
}



class PassengerSupervisorSpec extends TestKit(ActorSystem("PassengerSupervisorSpec")) with WordSpecLike with ImplicitSender {
  trait TestPassengerProvider extends PassengerProvider {
    override def newPassenger(callButton: ActorRef): Actor = new Actor {
      override def receive: Receive = {
        case m => callButton ! m
      }
    }
  }

  trait TestPassengerListProvider extends PassengerListProvider {
    override def passengers: Seq[String] = Seq("foo", "bar", "baz")
  }

  "PassengerSuper" should {
    "broadcast msgs" in {
      val a = TestActorRef[PassengerSupervisor with PassengerListProvider](Props(new PassengerSupervisor(testActor) with TestPassengerProvider with TestPassengerListProvider))
      val passengers = a.underlyingActor.passengers
      a ! GetPassengerBroadcaster
      val b = expectMsgType[PassengerBroadcaster].broadcaster
      b ! FastenSeatbelts
      passengers.foreach { _ =>
        expectMsg(FastenSeatbelts)
      }
      expectNoMsg(100.milliseconds)
      a ! GetPassengerBroadcaster
      expectMsg(PassengerBroadcaster(b))
    }
  }
}

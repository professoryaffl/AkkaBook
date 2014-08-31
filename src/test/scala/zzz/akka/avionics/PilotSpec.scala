package zzz.akka.avionics

import akka.actor._
import akka.testkit.{TestActorRef, TestProbe, ImplicitSender, TestKit}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import org.scalatest.{Matchers, WordSpecLike}
import zzz.akka.avionics.Pilots.ReadyToGo
import scala.concurrent.Await
import scala.concurrent.duration._
import akka.pattern.ask

class FakePilot extends Actor {
  override def receive: Receive = {
    case _ =>
  }
}

object PilotSpec {
  val copilotName="Mike"
  val pilotName="Paul"
  val configStr =
    s"""
       |zzz.akka.avionics.flightcrew.copilotName="$copilotName"
       |zzz.akka.avionics.flightcrew.pilotName="$pilotName"
     """.stripMargin
}

class PilotSpec extends TestKit(ActorSystem("PilotSpec", ConfigFactory.parseString(PilotSpec.configStr))) with ImplicitSender with WordSpecLike with Matchers {

  import PilotSpec._
  import Plane._

  def nilActor = TestProbe().ref
  val pilotPath = s"/user/TestPilots/$pilotName"
  val copilotPath = s"/user/TestPilots/$copilotName"

  def pilotsReadyToGo(): ActorRef = {
    implicit val askTimeout = Timeout(4.seconds)
    val a = system.actorOf(Props(new IsolatedStopSupervisor with OneForOneStrategyFactory {
      override def childStarter(): Unit = {
        context.actorOf(Props[FakePilot], pilotName)
        context.actorOf(Props(new Copilot(testActor, nilActor, nilActor)), copilotName)
      }
    }), "TestPilots")
    Await.result(a ? IsolatedLifeCycleSupervisor.WaitForStart, 3.seconds)
    system.actorFor(copilotPath) ! Pilots.ReadyToGo
    a
  }

  "Copilot" should {
    "take control when the Pilot dies" in {
      pilotsReadyToGo()
      system.actorFor(pilotPath) ! PoisonPill
      expectMsg(GiveMeControl)
      lastSender should be (system.actorFor(copilotPath))
    }
  }

  "Autopilot" should {
    "take control when the copilot dies" in {
      val copilot = TestProbe().ref
      val controls = TestProbe().ref
      val autopilot = TestActorRef[Autopilot](Props(new Autopilot(testActor)))
      autopilot ! ReadyToGo
      expectMsg(RequestCopilot)
      autopilot ! CopilotReference(copilot)
      system.stop(copilot)
      expectMsg(GiveMeControl)
      autopilot ! Controls(controls)
      autopilot.underlyingActor.controls should be (controls)
    }
  }


}

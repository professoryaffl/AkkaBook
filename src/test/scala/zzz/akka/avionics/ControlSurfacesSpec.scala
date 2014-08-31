package zzz.akka.avionics

import akka.actor.{Props, ActorSystem}
import akka.testkit.{TestActorRef, TestKit}
import org.scalatest.{Matchers, WordSpecLike}

class ControlSurfacesSpec extends TestKit(ActorSystem("ControlSurfacesSpec")) with WordSpecLike with Matchers {

  import ControlSurfaces._
  import Altimeter._

  val controlSurface = TestActorRef[ControlSurfaces](Props(new ControlSurfaces(testActor,testActor,testActor)))

  "ControlSurfaces" should {
    "respond to StickForward" in {
      controlSurface ! StickForward(0.5f)
      expectMsg(RateChange(-0.5f))
    }
    "respond to StickBack" in {
      controlSurface ! StickBack(0.3f)
      expectMsg(RateChange(0.3f))
    }
  }


}

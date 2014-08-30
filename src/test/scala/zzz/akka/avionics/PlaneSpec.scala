package zzz.akka.avionics

import akka.actor.{Props, ActorSystem}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import org.scalatest.{Matchers, WordSpecLike}


class PlaneSpec extends TestKit(ActorSystem("PlaneSpec")) with WordSpecLike with Matchers with ImplicitSender {

  import Plane._

  val plane = TestActorRef[Plane]

  "PlaneSpec" should {
    "Give control" in {
      plane ! GiveMeControl
      expectMsg(plane.underlyingActor.controls)
    }
  }

}

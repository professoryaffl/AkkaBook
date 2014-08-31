package zzz.akka.avionics

import akka.actor.{Props, ActorRef, ActorSystem}
import akka.testkit.{TestActorRef, TestProbe, TestFSMRef, TestKit}
import org.scalatest.{Matchers, WordSpecLike}
import zzz.akka.avionics.Altimeter.AltitudeUpdate
import zzz.akka.avionics.DrinkingBehaviour.FeelingLikeZaphod
import zzz.akka.avionics.HeadingIndicator.HeadingUpdate
import zzz.akka.avionics.Pilots.ReadyToGo


class FlyingBehaviourSpec extends TestKit(ActorSystem("FlyingBehaviourSpec")) with WordSpecLike with Matchers {
  import FlyingBehaviour._
  import Plane._

  def nilActor = TestProbe().ref
  val target = CourseTarget(0,0,0)

  def fsm(plane: ActorRef=nilActor,
          heading: ActorRef=nilActor,
          altimeter: ActorRef=nilActor) = {
    TestFSMRef(new FlyingBehaviour(plane,heading,altimeter))
  }

  "FlyingBehaviour" should {
    "start in the Idle state and with the Uninitialized data" in {
      val a = fsm()
      a.stateName should be (Idle)
      a.stateData should be (Uninitialized)
    }
  }

  "PreparingToFly state" should {
    "stay in PreparingToFly state when only a HeadingUpdate is received" in {
      val a = fsm()
      a ! Fly(target)
      a ! HeadingUpdate(20)
      a.stateName should be (PreparingToFly)
      val sd = a.stateData.asInstanceOf[FlightData]
      sd.status.alt should be (-1)
      sd.status.hdg should be (20)
    }

    "move to Flying state when all parts received" in {
      val a = fsm()
      a ! Fly(target)
      a ! HeadingUpdate(20)
      a ! AltitudeUpdate(30)
      a ! Controls(testActor)
      a.stateName should be (Flying)
      val sd = a.stateData.asInstanceOf[FlightData]
      sd.status.alt should be (30)
      sd.status.hdg should be (20)
    }
  }

  "transition to FLying state" should {
    "create adjust timer" in {
      val a = fsm()
      a.setState(PreparingToFly)
      a.setState(Flying)
      a.timerActive_?("Adjustment") should be (true)
    }
  }

  "Pilot.becomeZaphod" should {
    "send new calculators" in {
      val ref = TestActorRef[Pilot](Props(new Pilot(nilActor,nilActor,nilActor,nilActor) with FlyingProvider {
        override def newFlyingBehaviour(plane: ActorRef, heading: ActorRef, altimeter: ActorRef) = test
      }))
      ref ! ReadyToGo
      ref ! FeelingLikeZaphod
      expectMsgAllOf(
        NewElevatorCalculator(Pilot.zaphodCalcElevator),
        NewBankCalculator(Pilot.zaphodCalcAilerons)
      )
    }
  }

}

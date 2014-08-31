package zzz.akka.avionics

import akka.actor.FSM.SubscribeTransitionCallBack
import akka.actor.{FSM, Terminated, ActorRef, Actor}
import zzz.akka.avionics.Pilots.ReadyToGo
import zzz.akka.avionics.Plane.{CopilotReference, RequestCopilot, GiveMeControl, Controls}

object Pilots {
  case object ReadyToGo
  case object TakeControl
  case object RelinquishControl
}

object Pilot {
  import FlyingBehaviour._
  import ControlSurfaces._

  val tipsyCalcElevator: Calculator = { (target,status) =>
    val msg = calcElevator(target, status)
    msg match {
      case StickForward(amt) => StickForward(amt * 1.03f)
      case StickBack(amt) => StickBack(amt * 1.03f)
      case m => m
    }
  }
  val zaphodCalcElevator: Calculator = { (target,status) =>
    val msg = calcElevator(target, status)
    msg match {
      case StickForward(amt) => StickForward(1f)
      case StickBack(amt) => StickBack(1f)
      case m => m
    }
  }
  val tipsyCalcAilerons: Calculator = { (target,status) =>
    val msg = calcAilerons(target, status)
    msg match {
      case StickLeft(amt) => StickLeft(amt * 1.03f)
      case StickRight(amt) => StickRight(amt * 1.03f)
      case m => m
    }
  }
  val zaphodCalcAilerons: Calculator = { (target,status) =>
    val msg = calcAilerons(target, status)
    msg match {
      case StickLeft(amt) => StickLeft(1f)
      case StickRight(amt) => StickRight(1f)
      case m => m
    }
  }

  def apply(plane: ActorRef, autopilot: ActorRef, heading: ActorRef, altimeter: ActorRef) =
    new Pilot(plane, autopilot, heading, altimeter) with DrinkingProvider with FlyingProvider
}

class Pilot(plane: ActorRef, autopilot: ActorRef, heading: ActorRef, altimeter: ActorRef) extends Actor {

  this: DrinkingProvider with FlyingProvider =>

  import Pilots._
  import Pilot._
  import Pilot._
  import Altimeter._
  import ControlSurfaces._
  import DrinkingBehaviour._
  import FlyingBehaviour._
  import FSM._

  val copilotName = context.system.settings.config.getString("zzz.akka.avionics.flightcrew.copilotName")

  def setCourse(flyer: ActorRef): Unit = {
    flyer ! Fly(CourseTarget(20000, 250, System.currentTimeMillis + 30000))
  }


  override def preStart(): Unit = {
    context.actorOf(newDrinkingBehaviour(self), "DrinkingBehaviour")
    context.actorOf(newFlyingBehaviour(plane, heading, altimeter), "FlyingBehaviour")
  }

  def boostrap: Receive = {
    case ReadyToGo =>
      val copilot = context.actorFor(s"../$copilotName")
      val flyer = context.actorFor("FlyingBehaviour")
      flyer ! SubscribeTransitionCallBack(self)
      setCourse(flyer)
      context.become(sober(copilot, flyer))
  }

  def sober(copilot: ActorRef, flyer: ActorRef): Receive = {
    case FeelingSober =>
    case FeelingTipsy => becomeTipsy(copilot, flyer)
    case FeelingLikeZaphod => becomeZaphod(copilot, flyer)
  }

  def tipsy(copilot: ActorRef, flyer: ActorRef): Receive = {
    case FeelingSober => becomeSober(copilot, flyer)
    case FeelingTipsy =>
    case FeelingLikeZaphod => becomeZaphod(copilot, flyer)
  }

  def zaphod(copilot: ActorRef, flyer: ActorRef): Receive = {
    case FeelingSober => becomeSober(copilot, flyer)
    case FeelingTipsy => becomeTipsy(copilot, flyer)
    case FeelingLikeZaphod =>
  }

  def idle: Receive = {
    case _ =>
  }

  def becomeSober(copilot: ActorRef, flyer: ActorRef) = {
    flyer ! NewElevatorCalculator(calcElevator)
    flyer ! NewBankCalculator(calcAilerons)
    context.become(sober(copilot, flyer))
  }
  def becomeTipsy(copilot: ActorRef, flyer: ActorRef) = {
    flyer ! NewElevatorCalculator(tipsyCalcElevator)
    flyer ! NewBankCalculator(tipsyCalcAilerons)
    context.become(tipsy(copilot, flyer))
  }
  def becomeZaphod(copilot: ActorRef, flyer: ActorRef) = {
    flyer ! NewElevatorCalculator(zaphodCalcElevator)
    flyer ! NewBankCalculator(zaphodCalcAilerons)
    context.become(zaphod(copilot, flyer))
  }


  override def unhandled(msg: Any): Unit = {
    msg match {
      case Transition(_, _, Flying) =>
        setCourse(sender)
      case Transition(_, _, Idle) =>
        context.become(idle)
      case Transition(_,_,_) =>
      case CurrentState(_,_) =>
      case m => super.unhandled(m)
    }
  }

  def receive = boostrap

}

class Copilot(plane: ActorRef, autopilot: ActorRef, altimeter: ActorRef) extends Actor {
  import Pilots._

  var controls = context.system.deadLetters
  var pilot = context.system.deadLetters
  val pilotName = context.system.settings.config.getString("zzz.akka.avionics.flightcrew.pilotName")

  def receive = {
    case ReadyToGo =>
      pilot = context.actorFor("../" + pilotName)
      context.watch(pilot)
    case Terminated(_) =>
      plane ! GiveMeControl
  }
}

class Autopilot(plane: ActorRef) extends Actor {
  var copilot = context.system.deadLetters
  var controls = context.system.deadLetters

  override def receive: Receive = {
    case ReadyToGo =>
      plane ! RequestCopilot
    case CopilotReference(cp) =>
      copilot = cp
      context.watch(cp)
    case Terminated(_) =>
      plane ! GiveMeControl
    case Controls(c) =>
      controls = c
  }
}

trait PilotProvider {
  def newPilot(plane: ActorRef, autopilot: ActorRef, heading: ActorRef, altimeter: ActorRef): Actor = Pilot(plane, autopilot, heading, altimeter)
  def newCopilot(plane: ActorRef, autopilot: ActorRef, altimeter: ActorRef): Actor = new Copilot(plane, autopilot, altimeter)
  def newAutopilot(plane: ActorRef): Actor = new Autopilot(plane)
}
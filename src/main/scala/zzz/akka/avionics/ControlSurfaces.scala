package zzz.akka.avionics

import akka.actor.{Actor, ActorRef}

object ControlSurfaces{
  case class StickBack(amount: Float)
  case class StickForward(amount: Float)
  case class StickLeft(amount: Float)
  case class StickRight(amount: Float)
  case class HasControl(somePilot: ActorRef)
}

class ControlSurfaces(plane: ActorRef, altimeter: ActorRef, heading: ActorRef) extends Actor{
  import zzz.akka.avionics.Altimeter._
  import zzz.akka.avionics.HeadingIndicator._
  import zzz.akka.avionics.ControlSurfaces._


  override def receive: Receive = controlledBy(context.system.deadLetters)

  def controlledBy(somePilot: ActorRef): Receive = {
    case StickBack(amount) if sender == somePilot =>
      altimeter ! RateChange(amount)
    case StickForward(amount) if sender == somePilot =>
      altimeter ! RateChange(-1 * amount)
    case StickLeft(amount) if sender == somePilot =>
      heading ! BankChange(amount)
    case StickRight(amount) if sender == somePilot =>
      heading ! BankChange(-1 * amount)
    case HasControl(entity) if sender == plane =>
      context.become(controlledBy(entity))
  }

}

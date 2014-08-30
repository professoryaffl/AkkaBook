package zzz.akka.avionics

import akka.actor.{Actor, ActorRef}

object ControlSurfaces{
  case class StickBack(amount: Float)
  case class StickForward(amount: Float)
}

class ControlSurfaces(altimeter: ActorRef) extends Actor{
  import zzz.akka.avionics.Altimeter._
  import zzz.akka.avionics.ControlSurfaces._


  def receive = {
    case StickBack(amount)    =>
      altimeter ! RateChange(amount)
    case StickForward(amount) =>
      altimeter ! RateChange(-1 * amount)
  }

}

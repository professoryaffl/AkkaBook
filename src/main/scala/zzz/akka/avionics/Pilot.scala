package zzz.akka.avionics

import akka.actor.{Terminated, ActorRef, Actor}
import zzz.akka.avionics.Pilots.ReadyToGo
import zzz.akka.avionics.Plane.{CopilotReference, RequestCopilot, GiveMeControl, Controls}

object Pilots {
  case object ReadyToGo
  case object RelinquishControl
}

class Pilot(plane: ActorRef, autopilot: ActorRef, var controls: ActorRef, altimeter: ActorRef) extends Actor {

  import Pilots._
  import Plane._

  var copilot = context.system.deadLetters
  val copilotName = context.system.settings.config.getString("zzz.akka.avionics.flightcrew.copilotName")

  def receive = {
    case ReadyToGo =>
      plane ! GiveMeControl
      copilot = context.actorFor("../" + copilotName)
    case Controls(controlSurfaces) =>
      controls = controlSurfaces
  }

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
  def newPilot(plane: ActorRef, autopilot: ActorRef, controls: ActorRef, altimeter: ActorRef): Actor = new Pilot(plane, autopilot, controls, altimeter)
  def newCopilot(plane: ActorRef, autopilot: ActorRef, altimeter: ActorRef): Actor = new Copilot(plane, autopilot, altimeter)
  def newAutopilot(plane: ActorRef): Actor = new Autopilot(plane)
}
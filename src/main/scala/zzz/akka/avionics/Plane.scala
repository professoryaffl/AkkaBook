package zzz.akka.avionics

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.Timeout
import zzz.akka.avionics.IsolatedLifeCycleSupervisor.WaitForStart
import scala.concurrent.Await
import scala.concurrent.duration._
import akka.pattern.ask


object Plane{
  case object GiveMeControl
  case object RequestCopilot
  case class Controls(controls: ActorRef)
  case class CopilotReference(controls: ActorRef)

  def apply() = new Plane with PilotProvider with AltimeterProvider with LeadFlightAttendantProvider
}

class Plane extends Actor with ActorLogging{
  this: PilotProvider with AltimeterProvider with LeadFlightAttendantProvider =>


  import zzz.akka.avionics.Altimeter.AltitudeUpdate
  import zzz.akka.avionics.EventSource.RegisterListener
  import zzz.akka.avionics.Plane._


  val altimeter = context.actorOf(Props(Altimeter()), "Altimeter")
  val controls = context.actorOf(Props(new ControlSurfaces(altimeter)), "ControlSurfaces")
  val cfgstr = "zzz.akka.avionics.flightcrew"
  val config = context.system.settings.config
  val pilotName = config.getString(s"$cfgstr.pilotName")
  val copilotName = config.getString(s"$cfgstr.copilotName")
  val attendantName = config.getString(s"$cfgstr.leadAttendantName")

  def actorForPilots(name: String) = context.actorFor(s"Pilots/$name")

  override def preStart = {
    import EventSource.RegisterListener
    import Pilots.ReadyToGo

    startEquipment()
    startPeople()
    actorForControls("Altimeter") ! RegisterListener(self)
    actorForPilots(pilotName) ! ReadyToGo
    actorForPilots(copilotName) ! ReadyToGo
  }

  def receive = {
    case RequestCopilot =>
      sender ! CopilotReference(actorForPilots(copilotName))
    case GiveMeControl =>
      log info("Plane giving control")
      sender ! Controls(controls)
    case AltitudeUpdate(alt) =>
      println(s"Altitude is now $alt")
  }

  implicit val askTimeout = Timeout(1.second)
  def startEquipment(): Unit = {
    val controls = context.actorOf(
      Props(new IsolatedResumeSupervisor with OneForOneStrategyFactory {
        override def childStarter(): Unit = {
          val alt = context.actorOf(Props(newAltimeter), "Altimeter")
          context.actorOf(Props(newAutopilot(self)), "Autopilot")
          context.actorOf(Props(new ControlSurfaces(alt)), "ControlSurfaces")
        }
      }), "Equipment"
    )
    Await.result(controls ? WaitForStart, 1.second)
  }

  def actorForControls(name: String) = context.actorFor(s"Equipment/$name")

  def startPeople(): Unit = {
    val plane = self
    val controls = actorForControls("ControlSurfaces")
    val autopilot = actorForControls("Autopilot")
    val altimeter = actorForControls("Altimeter")
    val people = context.actorOf(
      Props(new IsolatedStopSupervisor with OneForOneStrategyFactory {
        override def childStarter(): Unit = {
          context.actorOf(Props(newPilot(plane, autopilot, controls, altimeter)), pilotName)
          context.actorOf(Props(newCopilot(plane, autopilot, altimeter)), copilotName)
        }
      }), "Pilots"
    )
    context.actorOf(Props(newLeadFlightAttendant), attendantName)
    Await.result(people ? WaitForStart, 1.second)
  }

}

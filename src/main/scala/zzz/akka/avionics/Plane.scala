package zzz.akka.avionics

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.routing.FromConfig
import akka.util.Timeout
import zzz.akka.avionics.IsolatedLifeCycleSupervisor.WaitForStart
import zzz.akka.avionics.StatusReporter.ReportStatus
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import akka.pattern.ask


object Plane{
  case object GetAllInstrumentStatus
  case object GiveMeControl
  case object LostControl
  case object RequestCopilot
  case class Controls(controls: ActorRef)
  case class CopilotReference(controls: ActorRef)

  def apply() = new Plane with PilotProvider with AltimeterProvider with LeadFlightAttendantProvider
}

class Plane extends Actor with ActorLogging{
  this: PilotProvider with AltimeterProvider with LeadFlightAttendantProvider =>

  import Altimeter._
  import HeadingIndicator._
  import Plane._
  import Pilots._


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

  val instruments = Vector(actorForControls("Altimeter"), actorForControls("HeadingIndicator"))

  implicit val ec = context.dispatcher

  def receive = {
    case GetCurrentHeading =>
      actorForControls("HeadingIndicator") forward GetCurrentHeading
    case GetCurrentAltitude =>
      actorForControls("Altimeter") forward GetCurrentAltitude
    case GetAllInstrumentStatus =>
      import StatusReporter._
      import akka.pattern.pipe
      val instrumentFutures = instruments map { i => (i ? ReportStatus).mapTo[Status] }
      val f = Future.sequence(instrumentFutures)
      f map { results =>
        if (results.contains(StatusBAD)) StatusBAD
        else if (results.contains(StatusNotGreat)) StatusNotGreat
        else StatusOK
      } pipeTo sender
    case RequestCopilot =>
      sender ! CopilotReference(actorForPilots(copilotName))
    case GiveMeControl =>
      log info("Plane giving control")
      sender ! Controls(actorForControls("ControlSurfaces"))
    case AltitudeUpdate(alt) =>
      println(s"Altitude is now $alt")
    case LostControl =>
      actorForControls("Autopilot") ! TakeControl
  }

  implicit val askTimeout = Timeout(1.second)
  def startEquipment(): Unit = {
    val controls = context.actorOf(
      Props(new IsolatedResumeSupervisor with OneForOneStrategyFactory {
        override def childStarter(): Unit = {
          val alt = context.actorOf(Props(newAltimeter), "Altimeter")
          val heading = context.actorOf(Props(HeadingIndicator()), "HeadingIndicator")
          context.actorOf(Props(newAutopilot(self)), "Autopilot")
          context.actorOf(Props(new ControlSurfaces(self, alt, heading)), "ControlSurfaces")
        }
      }), "Equipment"
    )
    Await.result(controls ? WaitForStart, 1.second)
  }

  def actorForControls(name: String) = context.actorFor(s"Equipment/$name")

  def startPeople(): Unit = {
    val plane = self
    val autopilot = actorForControls("Autopilot")
    val altimeter = actorForControls("Altimeter")
    val heading = actorForControls("HeadingIndicator")
    val pilots = context.actorOf(
      Props(new IsolatedStopSupervisor with OneForOneStrategyFactory {
        override def childStarter(): Unit = {
          context.actorOf(Props(newPilot(plane, autopilot, heading, altimeter)), pilotName)
          context.actorOf(Props(newCopilot(plane, autopilot, altimeter)), copilotName)
        }
      }), "Pilots"
    )
    Await.result(pilots ? WaitForStart, 1.second)
    val leadAttendant =
      context.actorOf(Props(newLeadFlightAttendant).withRouter(FromConfig()), "FlightAttendantRouter")
    val people =
      context.actorOf(Props(new IsolatedStopSupervisor with OneForOneStrategyFactory {
        def childStarter(): Unit = {
          context.actorOf(Props(PassengerSupervisor(leadAttendant)), "Passengers")
        }
      }))
    Await.result(people ? WaitForStart, 1.second)
  }

}

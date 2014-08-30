package zzz.akka.avionics

import akka.actor.{Props, Actor, ActorRef}
import zzz.akka.avionics.LeadFlightAttendant.GetFlightAttendant

import scala.util.Random


trait AttendantCreationPolicy {
  val numberOfAttendants = 8
  def createAttendant = FlightAttendant()
}

trait LeadFlightAttendantProvider {
  def newLeadFlightAttendant = LeadFlightAttendant()
}


object LeadFlightAttendant {
  case object GetFlightAttendant
  case class Attendant(a: ActorRef)
  def apply() = new LeadFlightAttendant with AttendantCreationPolicy
}


class LeadFlightAttendant extends Actor{
  this: AttendantCreationPolicy =>

  import LeadFlightAttendant._

  override def preStart(): Unit = {
    import scala.collection.JavaConverters._
    val attendantNames=context.system.settings.config.getStringList("zzz.akka.avionics.flightcrew.attendantNames").asScala
    attendantNames take numberOfAttendants foreach { name =>
      context.actorOf(Props(createAttendant), name)
    }
  }

  def randomAttendant(): ActorRef = {
    context.children.take(Random.nextInt(numberOfAttendants) + 1).last
  }

  override def receive: Receive = {
    case GetFlightAttendant =>
      sender ! Attendant(randomAttendant())
    case m =>
      randomAttendant() forward m
  }
}

object FlightAttendantPathChecker {
  def main(args: Array[String]): Unit = {
    val system = akka.actor.ActorSystem("PlaneSim")
    val lead = system.actorOf(Props(new LeadFlightAttendant with AttendantCreationPolicy), system.settings.config.getString("zzz.akka.avionics.flightcrew.leadAttendantName"))
    Thread.sleep(2000)
    system.shutdown()
  }
}

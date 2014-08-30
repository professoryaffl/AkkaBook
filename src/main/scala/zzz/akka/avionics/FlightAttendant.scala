package zzz.akka.avionics

import akka.actor.Actor

import scala.util.Random
import scala.concurrent.duration._

trait AttendantResponsiveness {
  val maxResponseTimeMS: Int
  def responseDuration = Random.nextInt(maxResponseTimeMS).millis
}

object FlightAttendant {

  case class GetDrink(drinkname: String)
  case class Drink(drinkname: String)

  def apply() = new FlightAttendant with AttendantResponsiveness {
    val maxResponseTimeMS = 300000
  }

}

class FlightAttendant extends Actor {
  this: AttendantResponsiveness =>

  import FlightAttendant._

  implicit val ec = context.dispatcher

  override def receive: Receive = {
    case GetDrink(dn) =>
      context.system.scheduler.scheduleOnce(responseDuration, sender, Drink(dn))
  }
}
package zzz.akka.avionics

import akka.actor.{Actor, ActorLogging}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


object Altimeter{
  case class RateChange(amount: Float)
  case class AltitudeUpdate(altitude: Double)

  def apply() = new Altimeter with ProductionEventSource
}

class Altimeter extends Actor with ActorLogging {
  this: EventSource =>

  import zzz.akka.avionics.Altimeter._

  val ceiling = 43000
  val maxRateOfClimb = 5000
  var rateOfClimb = 0f
  var altitude = 0d
  var lastTick = System.currentTimeMillis()
  var ticker =  context.system.scheduler.schedule(100.millis, 100.millis, self, Tick)

  case object Tick

  def altimeterReceive: Receive = {
    case RateChange(amount) =>
      rateOfClimb = amount.min(1.0f).max(-1.0f) * maxRateOfClimb
      log.info(s"Altimeter changed rate of climb to $rateOfClimb")
    case Tick =>
      val tick = System.currentTimeMillis
      altitude = altitude + rateOfClimb * ((tick - lastTick) / 60000.0)
      lastTick = tick
      sendEvent(AltitudeUpdate(altitude))
  }

  def receive = eventSourceReceive orElse altimeterReceive

  override def postStop(): Unit = ticker.cancel
}

trait AltimeterProvider {
  def newAltimeter: Actor = Altimeter()
}

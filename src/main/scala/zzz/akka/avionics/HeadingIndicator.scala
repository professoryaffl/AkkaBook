package zzz.akka.avionics

import akka.actor.{ActorLogging, Actor}
import zzz.akka.avionics.StatusReporter.Status
import scala.concurrent.duration._

object HeadingIndicator {
  case class BankChange(amount: Float)
  case class HeadingUpdate(amount: Float)
  case object GetCurrentHeading

  case class CurrentHeading(heading: Float)

  def apply() = new HeadingIndicator with ProductionEventSource
}

trait HeadingIndicator extends Actor with ActorLogging with StatusReporter {
  this: EventSource =>

  import HeadingIndicator._
  import context._

  case object Tick

  val maxDegPerSec = 5

  val ticker = system.scheduler.schedule(100.millis, 100.millis, self, Tick)

  var lastTick: Long = System.currentTimeMillis()

  var rateOfBank = 0f
  var heading = 0f

  import StatusReporter._
  override def currentStatus: Status = StatusOK

  def headingIndicatorReceive: Receive = {
    case GetCurrentHeading =>
      sender ! CurrentHeading(heading)
    case BankChange(amount) =>
      rateOfBank = amount.min(1.0f).max(-1.0f)
    case Tick =>
      val tick = System.currentTimeMillis()
      val timeDelta = (tick - lastTick) / 1000f
      val degs = rateOfBank * maxDegPerSec
      heading = (heading + (360 + (timeDelta * degs))) % 360
      lastTick = tick
      sendEvent(HeadingUpdate(heading))
  }

  def receive = statusReceive orElse eventSourceReceive orElse headingIndicatorReceive

  override def postStop(): Unit = ticker.cancel
}

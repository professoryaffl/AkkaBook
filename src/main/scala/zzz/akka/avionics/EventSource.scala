package zzz.akka.avionics

import akka.actor.{Actor, ActorRef}

object EventSource {
  case class RegisterListener(listener: ActorRef)
  case class UnregisterListener(listener: ActorRef)
}

trait EventSource {
  def sendEvent[T](event: T): Unit
  def eventSourceReceive: Actor.Receive
}

trait ProductionEventSource extends EventSource { this: Actor =>

  import zzz.akka.avionics.EventSource._

  var listeners = Vector.empty[ActorRef]

  def sendEvent[T](event: T): Unit = listeners foreach {
    _ ! event
  }

  def eventSourceReceive: Receive = {
    case RegisterListener(l) => listeners = listeners :+ l
    case UnregisterListener(l) => listeners = listeners filter { _ != l }
  }

}

package zzz.akka.avionics

import akka.actor.{Actor, ActorLogging, Props}


object Plane{
  case object GiveMeControl
}

class Plane extends Actor with ActorLogging{
  import zzz.akka.avionics.Altimeter.AltitudeUpdate
  import zzz.akka.avionics.EventSource.RegisterListener
  import zzz.akka.avionics.Plane._


  val altimeter = context.actorOf(Props(Altimeter()), "Altimeter")
  val controls = context.actorOf(Props(new ControlSurfaces(altimeter)), "ControlSurfaces")


  override def preStart = {
    altimeter ! RegisterListener(self)
  }

  def receive = {
    case GiveMeControl =>
      log info("Plane giving control")
      sender ! controls
    case AltitudeUpdate(alt) =>
      println(s"Altitude is now $alt")
  }

}

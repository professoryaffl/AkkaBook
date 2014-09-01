package zzz.akka.avionics

import akka.actor.SupervisorStrategy.{Stop, Resume, Escalate}
import akka.actor._
import akka.routing.BroadcastRouter
import com.typesafe.config.ConfigList

import scala.concurrent.duration._


object Passenger {
  case object FastenSeatbelts
  case object UnfastenSeatbelts

  val SeatAssignment = """([\w\s_]+)-(\d+)-([A-Z])""".r
}

trait DrinkRequestProbability {
  val askThreshold = 0.9f
  val requestMin = 20.minutes
  val requestUpper = 30.minutes

  def randomishTime(): FiniteDuration = requestMin + scala.util.Random.nextInt(requestUpper.toMillis.toInt).millis
}

trait PassengerProvider {
  def newPassenger(callButton: ActorRef): Actor =
    new Passenger(callButton) with DrinkRequestProbability
}

class Passenger(callButton: ActorRef) extends Actor with ActorLogging {
  this: DrinkRequestProbability =>

  import Passenger._
  import FlightAttendant.{GetDrink, Drink}
  import scala.collection.JavaConverters._

  val r = scala.util.Random
  implicit val ec = context.dispatcher

  case object CallForDrink

  val SeatAssignment(myname, _, _) = self.path.name.replaceAllLiterally("_", " ")
  val drinks = context.system.settings.config.getStringList("zzz.akka.avionics.drinks").asScala.toIndexedSeq
  val scheduler = context.system.scheduler


  override def preStart(): Unit = {
    self ! CallForDrink
  }

  def maybeSendDrinkRequest(): Unit = {
    if (r.nextFloat() > askThreshold) {
      val drinkname = drinks(r.nextInt(drinks.length))
      callButton ! GetDrink(drinkname)
    }
    scheduler.scheduleOnce(randomishTime(), self, CallForDrink)
  }

  override def receive: Receive = {
    case CallForDrink => maybeSendDrinkRequest()
    case Drink(drinkname) => log.info(s"$myname received a $drinkname")
    case FastenSeatbelts => log.info(s"$myname fastening seatbelt")
    case UnfastenSeatbelts => log.info(s"$myname unfastening seatbelt")
  }
}

object PassengerSupervisor {
  case object GetPassengerBroadcaster
  case class PassengerBroadcaster(broadcaster: ActorRef)

  def apply(callButton: ActorRef) = new PassengerSupervisor(callButton) with PassengerProvider with ProductionPassengerListProvider
}

trait PassengerListProvider {
  def passengers: Seq[String]
}

trait ProductionPassengerListProvider extends PassengerListProvider {
  this: Actor =>

  import scala.collection.JavaConverters._
  import com.typesafe.config.ConfigList

  def passengers: Seq[String] = {
    val config = context.system.settings.config
    val passengers = config.getList("zzz.akka.avionics.passengers").asScala
    passengers.map { s=> s.asInstanceOf[ConfigList].unwrapped().asScala.mkString("-").replaceAllLiterally(" ", "_") }
  }
}

class PassengerSupervisor(callButton: ActorRef) extends Actor {
  this: PassengerProvider with PassengerListProvider =>

  import PassengerSupervisor._

  override def supervisorStrategy: SupervisorStrategy = OneForOneStrategy() {
    case _: ActorKilledException => Escalate
    case _: ActorInitializationException => Escalate
    case _ => Resume
  }

  case class GetChildren(forSomeone: ActorRef)
  case class Children(children: Iterable[ActorRef], childrenFor: ActorRef)

  override def preStart(): Unit = {
    context.actorOf(Props(new Actor {
      val config = context.system.settings.config
      override def supervisorStrategy: SupervisorStrategy = OneForOneStrategy() {
        case _: ActorKilledException => Escalate
        case _: ActorInitializationException => Escalate
        case _ => Stop
      }
      override def preStart(): Unit = passengers.foreach { context.actorOf(Props(newPassenger(callButton)), _) }
      def receive = {
        case GetChildren(forSomeone: ActorRef) =>
          sender ! Children(context.children, forSomeone)
      }
    }), "PassengerSupervisor")
  }

  def noRouter: Receive = {
    case GetPassengerBroadcaster =>
      val passengers = context.actorFor("PassengerSupervisor")
      passengers ! GetChildren(sender)
    case Children(passengers, destinedFor) =>
      val router = context.actorOf(Props().withRouter(BroadcastRouter(passengers.toSeq)), "Passengers")
      destinedFor ! PassengerBroadcaster(router)
      context.become(withRouter(router))
  }

  def withRouter(router: ActorRef): Receive = {
    case GetPassengerBroadcaster =>
      sender ! PassengerBroadcaster(router)
    case Children(_, destinedFor) =>
      destinedFor ! PassengerBroadcaster(router)
  }

  def receive = noRouter
}
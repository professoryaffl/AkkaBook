package zzz.akka.avionics

import akka.actor.{Props, FSM, Actor, ActorRef}
import scala.concurrent.duration._

trait FlyingProvider {
  def newFlyingBehaviour(plane: ActorRef, heading: ActorRef, altimeter: ActorRef): Props =
    Props(new FlyingBehaviour(plane, heading, altimeter))
}

object FlyingBehaviour {
  import ControlSurfaces._

  sealed trait State
  case object Idle extends State
  case object Flying extends State
  case object PreparingToFly extends State

  case class NewElevatorCalculator(f: Calculator)
  case class NewBankCalculator(f: Calculator)

  case class CourseTarget(alt: Double, hdg: Float, byMillis: Long)
  case class CourseStatus(alt: Double, hdg: Float, hdgSinceMs: Long, altSinceMs: Long)

  type Calculator = (CourseTarget, CourseStatus) => Any

  sealed trait Data
  case object Uninitialized extends Data
  case class FlightData(controls: ActorRef,
                        elevCalc: Calculator,
                        bankCalc: Calculator,
                        target: CourseTarget,
                        status: CourseStatus
                         ) extends Data

  case class Fly(target: CourseTarget)

  def currentMs = System.currentTimeMillis

  def calcElevator(target: CourseTarget, status: CourseStatus): Any = {
    val alt = (target.alt - status.alt).toFloat
    val dur = target.byMillis - status.altSinceMs
    if (alt < 0) StickForward((alt/dur) * -1) else StickBack(alt/dur)
  }

  def calcAilerons(target: CourseTarget, status: CourseStatus): Any = {
    import scala.math.{abs, signum}
    val diff = target.hdg - status.hdg
    val dur = target.byMillis - status.hdgSinceMs
    val amount = if (abs(diff) < 180) diff else signum(diff) * (abs(diff) - 360f)
    if (amount > 0) StickRight(amount/dur) else StickLeft((amount/dur) * -1)
  }
}

class FlyingBehaviour(plane: ActorRef, heading: ActorRef, altimeter: ActorRef) extends Actor with FSM[FlyingBehaviour.State, FlyingBehaviour.Data] {
  import FSM._
  import FlyingBehaviour._
  import Pilots._
  import Plane._
  import Altimeter._
  import HeadingIndicator._
  import EventSource._

  case object Adjust
  case object PrepareTimeout

  startWith(Idle, Uninitialized)

  when(Idle) {
    case Event(Fly(target), _) =>
      goto(PreparingToFly) using FlightData(
        context.system.deadLetters,
        calcElevator,
        calcAilerons,
        target,
        CourseStatus(-1,-1,0,0)
      )
  }

  onTransition {
    case Idle -> PreparingToFly =>
      plane ! GiveMeControl
      heading ! RegisterListener(self)
      altimeter ! RegisterListener(self)
      setTimer("PreparingTimeout", PrepareTimeout, 5.seconds, repeat=false)
  }

  when(PreparingToFly)(transform {
    case Event(HeadingUpdate(head), d: FlightData) =>
      stay using d.copy(status=d.status.copy(hdg=head,hdgSinceMs = currentMs))
    case Event(AltitudeUpdate(newAlt), d: FlightData) =>
      stay using d.copy(status=d.status.copy(alt=newAlt,altSinceMs = currentMs))
    case Event(Controls(ctrls), d: FlightData) =>
      stay using d.copy(controls = ctrls)
    case Event(PrepareTimeout, _) =>
      plane ! LostControl
      goto(Idle)
  } using {
    case s if prepComplete(s.stateData) =>
      cancelTimer("PreparingTimeout")
      s.copy(stateName = Flying)
  })

  onTransition {
    case PreparingToFly -> Flying =>
      setTimer("Adjustment", Adjust, 200.milliseconds, repeat=true)
  }

  when(Flying) {
    case Event(AltitudeUpdate(a), d: FlightData) =>
      stay using d.copy(status = d.status.copy(alt = a, altSinceMs = currentMs))
    case Event(HeadingUpdate(h), d: FlightData) =>
      stay using d.copy(status = d.status.copy(hdg = h, hdgSinceMs = currentMs))
    case Event(Adjust, d:FlightData) =>
      stay using adjust(d)
    case Event(NewBankCalculator(f), d:FlightData) =>
      stay using d.copy(bankCalc = f)
    case Event(NewElevatorCalculator(f), d:FlightData) =>
      stay using d.copy(elevCalc = f)
  }

  onTransition {
    case Flying -> _ =>
      cancelTimer("Adjustment")
  }

  onTransition {
    case _ -> Idle =>
      heading ! UnregisterListener(self)
      altimeter ! UnregisterListener(self)
  }

  whenUnhandled {
    case Event(RelinquishControl, _) =>
      goto(Idle)
  }

  initialize

  def adjust(d: FlightData): FlightData = {
    val FlightData(c, elevCalc, bankCalc, t, s) = d
    c ! elevCalc(t,s)
    c ! bankCalc(t,s)
    d
  }

  def prepComplete(data: Data): Boolean = {
    data match {
      case FlightData(c,_,_,_,s) =>
        !c.isTerminated && s.hdg != -1f && s.alt != -1f
      case _ =>
        false
    }
  }
}

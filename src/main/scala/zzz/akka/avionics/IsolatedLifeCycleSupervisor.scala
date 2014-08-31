package zzz.akka.avionics

import akka.actor.SupervisorStrategy.{Stop, Escalate, Resume}
import akka.actor.{ActorKilledException, ActorInitializationException, SupervisorStrategy, Actor}

import scala.concurrent.duration.Duration


object IsolatedLifeCycleSupervisor {
  case object WaitForStart
  case object Started
}

trait IsolatedLifeCycleSupervisor extends Actor {
  import IsolatedLifeCycleSupervisor._

  def receive = {
    case WaitForStart =>
      sender ! Started

    case m => throw new Exception(s"Shouldn't call me: ${self.path.name}, $m")
  }

  def childStarter(): Unit

  final override def preStart(): Unit = { childStarter() }

  final override def postRestart(reason: Throwable): Unit = {}

  final override def preRestart(reason: Throwable, message: Option[Any]): Unit = {}
}

abstract class IsolatedResumeSupervisor(maxNrRetries: Int = -1, withinTimeRange: Duration = Duration.Inf) extends IsolatedLifeCycleSupervisor {
  this: SupervisionStrategyFactory =>
  override def supervisorStrategy: SupervisorStrategy = makeStrategy(maxNrRetries, withinTimeRange) {
    case _: ActorInitializationException => Stop
    case _: ActorKilledException => Stop
    case _: Exception => Resume
    case _ => Escalate
  }
}

abstract class IsolatedStopSupervisor(maxNrRetries: Int = -1, withinTimeRange: Duration = Duration.Inf) extends IsolatedLifeCycleSupervisor {
  this: SupervisionStrategyFactory =>
  override def supervisorStrategy: SupervisorStrategy = makeStrategy(maxNrRetries, withinTimeRange) {
    case _: ActorInitializationException => Stop
    case _: ActorKilledException => Stop
    case _: Exception => Stop
    case _ => Escalate
  }
}
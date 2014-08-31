package zzz.akka.avionics

import akka.actor.{Props, Actor, ActorRef}

import scala.concurrent.duration._

trait DrinkingProvider {
  def newDrinkingBehaviour(drinker: ActorRef): Props =
    Props(DrinkingBehaviour(drinker))
}

object DrinkingBehaviour {
  case class LevelChanged(level: Float)
  case object FeelingSober
  case object FeelingTipsy
  case object FeelingLikeZaphod

  def apply(drinker: ActorRef) = new DrinkingBehaviour(drinker) with DrinkingResolution
}

trait DrinkingResolution {
  import scala.util.Random

  def initialSobering: FiniteDuration = 1.second
  def soberingInterval: FiniteDuration = 1.second
  def drinkInterval(): FiniteDuration = Random.nextInt(300).seconds
}

class DrinkingBehaviour(drinker: ActorRef) extends Actor {

  this: DrinkingResolution  =>

  import DrinkingBehaviour._

  var currentLevel = 0f
  val scheduler = context.system.scheduler
  implicit val ec = context.system.dispatcher
  val sobering = scheduler.schedule(initialSobering, soberingInterval, self, LevelChanged(-0.0001f))

  override def postStop(): Unit = sobering.cancel()

  override def preStart(): Unit = drink()

  def drink() = scheduler.scheduleOnce(drinkInterval(), self, LevelChanged(0.005f))

  override def receive: Receive = {
    case LevelChanged(amount) =>
      currentLevel = (currentLevel + amount).max(0f)
      drinker ! (if (currentLevel <= 0.01) {
        drink()
        FeelingSober
      } else if (currentLevel <= 0.03) {
        drink()
        FeelingTipsy
      } else FeelingLikeZaphod)
  }
}

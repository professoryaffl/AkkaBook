package zzz.akka.avionics

import akka.actor.{Props, ActorSystem, Actor}
import akka.routing.{Destination, RouterConfig}
import akka.testkit.{ExtractRoute, ImplicitSender, TestKit}
import org.scalatest.{Matchers, BeforeAndAfterAll, WordSpecLike}


class TestRoutee extends Actor {
  override def receive: Receive = Actor.emptyBehavior
}
class TestPassenger extends Actor {
  override def receive: Receive = Actor.emptyBehavior
}

class SectionSpecificAttendantRouterSpec extends TestKit(ActorSystem("SectionSpecificAttendantRouterSpec")) with ImplicitSender with WordSpecLike with BeforeAndAfterAll with Matchers {
  override protected def afterAll(): Unit = system.shutdown()

  def newRouter(): RouterConfig = new SectionSpecificAttendantRouter with FlightAttendantProvider {
    override def newFlightAttendant(): Actor = new TestRoutee
  }

  def passengerWithRow(row: Int) = system.actorOf(Props[TestPassenger], s"Someone-$row-C")

  val passengers = (1 to 25).map(passengerWithRow)

  "SectionSpecificAttendantRouter" should {
    "route consistently" in {
      val router = system.actorOf(Props[TestRoutee].withRouter(newRouter()))
      val route = ExtractRoute(router)
      val routeA: Iterable[Destination] = passengers.slice(0,10).flatMap { p =>
        route(p, "Hi")
      }
      routeA.tail.forall { dest =>
        dest.recipient == routeA.head.recipient
      } should be (true)
      val routeAB: Iterable[Destination] = passengers.slice(9,11).flatMap { p =>
        route(p, "Hi")
      }
      routeAB.head should not be (routeAB.tail.head)
      val routeB: Iterable[Destination] = passengers.slice(10,20).flatMap { p =>
        route(p, "Hi")
      }
      routeB.tail.forall { dest =>
        dest.recipient == routeB.head.recipient
      } should be (true)
    }
  }
}

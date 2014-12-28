package collapse

import java.net.URI

import akka.actor.ActorSystem
import collapse.strategy._
import io.backchat.hookup._


object Main extends App {

  val system = ActorSystem("test")

  val username = "test"
  val uri = URI.create(s"ws://tetrisj.jvmhost.net:12270/codenjoy-contest/ws?user=$username")

  val boardRegexp = "^board=(.*)$".r

  val strategy = new PriorityGroupStrategy(
    BestScoreStrategy,
    RandomActionStrategy
  )

  new DefaultHookupClient(HookupClientConfig(uri)) {
    override def receive = {
      case Disconnected(_) =>
        println("The websocket disconnected.")
      case TextMessage(text) =>
        println(s"<-- $text")

        text match {
          case boardRegexp(boardStr) =>
            val action = strategy.actt(Board.parse(boardStr))
            val (x, y) = action.from
            val dir = Direction.strRepr(action.direction)

            val msg = s"ACT($x,$y),$dir"

            println(s"--> $msg")

            send(msg)
          case _ =>
            println("Can't parse input message")
        }
    }

    connect() onSuccess {
      case _ =>
        println("Successfully connected to server")
    }
  }

}

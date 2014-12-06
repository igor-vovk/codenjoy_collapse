package collapse

import java.net.URI

import akka.actor.ActorSystem
import collapse.strategy.{Direction, RandomActionStrategy, Strategy}
import io.backchat.hookup.{TextMessage, HookupClientConfig, DefaultHookupClient}
import io.backchat.hookup.HookupClient.Receive


object Main extends App {

  val system = ActorSystem("test")

  val username = "test"
  val uri = URI.create(s"ws://tetrisj.jvmhost.net:12270/codenjoy-contest/ws?user=$username")

  val boardRegexp = "^board=(.*)$".r

  val strategy: Strategy = new RandomActionStrategy

  new DefaultHookupClient(HookupClientConfig(uri)) {
    override def receive: Receive = {
      case TextMessage(text) =>
        println(s"<-- $text")

        text match {
          case boardRegexp(boardStr) =>
            val action = strategy.act(Board.parse(boardStr))

            val msg = s"ACT(${action.coords._1},${action.coords._2}),${Direction.strRepr(action.direction)}"

            println(s"--> $msg")

            send(msg)
          case _ =>
            println("Can't parse input message")
        }
    }

    connect() onSuccess {
      case _ =>
        println("Successfuly connected to websocket server")
    }
  }

}

package collapse

import java.net.URI

import akka.actor.ActorSystem
import io.backchat.hookup.{TextMessage, HookupClientConfig, DefaultHookupClient}
import io.backchat.hookup.HookupClient.Receive


object Main extends App {

  val system = ActorSystem("test")

  val username = "test"
  val uri = URI.create(s"ws://tetrisj.jvmhost.net:12270/codenjoy-contest/ws?user=$username")

  val boardRegexp = "^board=(.*)$".r

  new DefaultHookupClient(HookupClientConfig(uri)) {
    override def receive: Receive = {
      case TextMessage(text) =>
        println(s"--> $text")

        text match {
          case boardRegexp(board) =>
            println(Board.parse(board))
        }
    }

    connect() onSuccess {
      case _ =>
        println("Successfuly connected to websocket server")
    }
  }

}

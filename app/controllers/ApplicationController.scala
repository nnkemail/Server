package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._
import model._
import akka.actor._
import model.Util.util._
import play.api.Play.current

class ApplicationController extends Controller {
  val system = ActorSystem("ServerSystem", settings.systemActorConfig)
  val serverActor = system.actorOf(ServerActor.props(), "ServerActor")

  def index = Action {request =>
    Ok("Dotti club node")
  }

  def socketGame = WebSocket.acceptWithActor[JsValue, JsValue] {request => out =>
    println("socket game")
    println(request.session)
    
    request.session.get("userID") match {
      case Some(userID) =>
        println("Logged user " + userID)
        request.session.get("nick") match { 
          case Some(nick) => PlayerActor.props(out, serverActor, Some(userID), nick)
          case None => PlayerActor.props(out, serverActor, Some(userID), "")
        }
      case None =>
        request.session.get("nick") match { 
          case Some(nick) => PlayerActor.props(out, serverActor, None, nick) 
          case None => PlayerActor.props(out, serverActor, None, "")
        }
    }
  }
}

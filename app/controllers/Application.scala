package controllers

import play.api._
import play.api.mvc._
import play.api.db._
import play.api.Play.current
import anorm._
import views.html.defaultpages.badRequest
import play.api.data._
import play.api.data.Forms._
import scala.xml.NodeSeq
import java.sql.Connection

object Application extends Controller {
  
  def index = Action {
    
    DB.withConnection { implicit c => 
      
      val gameSelect = SQL("select * from game")
      
      val games = gameSelect().toList
      
      Ok(views.html.index("Your new application is ready. " + games.size))
    }
    
  }
  
  val newRoomForm = Form(
	tuple(
	    "name" -> nonEmptyText,
	    "color" -> text
	)
  )
  
  def newroom = Action {
    
    Ok(views.html.newroom(newRoomForm))    
  }
  
  def createroom = Action { 
    implicit request =>
    
  	newRoomForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.newroom(formWithErrors)),
      {
        case (name, color) => {
        
	        DB.withConnection{ implicit c =>
	          
		        val gameId = SQL("insert into game(" + (color match {
		          case "red" => "red_player"
		          case "yellow" => "yellow_player"
		        }) + ") values ({name})")
		        .on("name" -> name)
		        .executeInsert().get
		        
		        Redirect("/game/" + gameId).withSession(
		            "name" -> name,
		            "gameId" -> gameId.toString
		        )
	        }
        }
      }
    )
  }
  
  def game(id: Long) = Action { implicit request =>
    
    session.get("gameId") match {
      case Some(gameId) => {
    	  if (!id.equals(gameId.toLong)) Redirect("/") 
    	  else Ok(views.html.game())
      }
      case None => Redirect("/")
    } 
  }
  
}
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
import play.api.libs.iteratee.Enumerator
import play.api.libs.Comet
import play.api.libs.{ Comet }
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import akka.util.duration._
import play.api.libs.json._
import anorm.SqlRow

object Application extends Controller {
  
  def index = Action {
    
    DB.withConnection { implicit c => 
      
      val gameSelect = SQL("select * from game")
      
      val games = gameSelect().toList
      
      Ok(views.html.index(games))
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
		            "gameId" -> gameId.toString,
		            "color" -> color
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
    	  else Ok(views.html.game(gameId.toLong, session.get("name").get, session.get("color").get))
      }
      case None => Redirect("/")
    } 
  }
  
  val joinGameForm = Form(
      tuple(
          "name" -> nonEmptyText,
          "gameId" -> number
      )
  )
  
  def joinGame(id: Long) = Action {
    
    DB.withConnection { implicit c =>
      
      val room = SQL("select * from game where id = {id}").on("id" -> id).apply().head
      
      Ok(views.html.joingame(room.asMap, joinGameForm))
      
    }
  }
  
  def enterGame(id: Long) = Action { implicit request => DB.withConnection { implicit c =>
        val room = SQL("select * from game where id = {id}").on("id" -> id).apply().head
    
	    joinGameForm.bindFromRequest.fold(
	        formWithErrors => BadRequest(views.html.joingame(room.asMap, formWithErrors)),
	        {
	          case (name, gameId) => {
	            // TODO compare id with gameId
	            
	            val playerColor = room.asMap("game.red_player") match {
	                case Some(_) => "yellow"
	                case None => "red"
	            }
	            
	            SQL("update game set " + playerColor + "_player = {playerName} where id = {id}")
	            	.on(
	            	    "playerName" -> name,
	            	    "id" -> gameId
	            	).executeUpdate()
	            	
	            	
	            Redirect("/game/" + gameId).withSession(
		            "name" -> name,
		            "gameId" -> gameId.toString,
		            "color" -> playerColor
		        )
	          }
	        }
	    )
    
  	}
  }
  
  def clock(gameId: Long): Enumerator[String] = {
    
    def status: String = DB.withConnection { implicit c =>
    	val game = SQL("select * from game where id = {id}").on("id" -> gameId).apply().head
    	
    	Pair(game.asMap("game.red_player"), game.asMap("game.yellow_player")) match {
    	  case (Some(_), Some(_)) => "Player joined"
    	  case _ => "Game will start automatically as soon as second player joins"
    	}
    }
    
    Enumerator.fromCallback { () =>
      Promise.timeout(Some(status), 1000 milliseconds)
    }
  }
  
  def liveClock(id: Long) = Action {
    Ok.stream(clock(id) &> Comet(callback = "parent.clockChanged"))
  }
  
  def playGame(id: Long, color: String) = Action {
    
    DB.withConnection { implicit c =>
      
    	Ok(views.html.playgame(id, color))
    }
    
    
  }
  
  def getColorByName(id: Long, name: String) = {
    DB.withConnection { implicit c =>
      val game = SQL("select * from game where id = {id}").on("id" -> id).apply().head
      
      game.asMap("game.red_player") match {
        case Some(redname) => if (redname.equals(name)) "red" else "yellow"
      }
    }
  }
  
  def gameSocket = WebSocket.async[JsValue] { request =>
    Akka.future {
      val out = Enumerator.imperative[JsValue]()
	  val in = Iteratee.foreach[JsValue]{ jsVal =>
	    
	    val gameId = (jsVal \ "gameId").as[Long]
	    
	    val color = (jsVal \ "color").as[String]
	    val column = (jsVal \ "column").asOpt[String]
	    
	    DB.withConnection { implicit c =>
	    	
	      (column) match {
	        case (Some(colnum)) => SQL("insert into move(game_id, color, colnum) values ({id}, {color}, {colnum})")
				.on(
				    "id" -> gameId,
				    "color" -> color,
				    "colnum" -> colnum
				).executeInsert()
				
	        case (None) => println("board refresh from " + color)
	      }
				
			val moves = SQL("select color, colnum from move where game_id = {id} order by id asc").on("id" -> gameId).apply().toList.map(row => {
				  val color: String = anormField(row, "move.color").asInstanceOf[String]
				  val column: Integer = anormField(row, "move.colnum").asInstanceOf[Integer]
				  Json.toJson(Map(
				      "color" -> Json.toJson(color),
				      "column" -> Json.toJson(column.toLong)
				      ))
			    })
			
			out.push(Json.toJson(moves))
	    }
	    
	  }
	  
	  (in, out)
	}
  }
  
  def anormField(row: SqlRow, field: String) = {
	  val mapped = row.asMap
	  mapped(field) match {
	    case Some(value) => value
	  }
  }
}
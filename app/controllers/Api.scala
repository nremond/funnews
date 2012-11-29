package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._
import models._
import models.Users.Login

import org.codehaus.jackson.node.ObjectNode;

object Api extends Controller {

  val loginForm = Form(mapping(
    "username" -> nonEmptyText,
    "password" -> nonEmptyText)(Login.apply)(Login.unapply))

  def login = Action { implicit request =>

    loginForm.bindFromRequest.fold(
      formWithErrors => errResult("Username and password are two required fields."),

      user => Users.checkUserCredentials(user.username, user.password) match {
        case None => errResult("No match for the specified username / password pair.")
        case Some(authDetails) => Ok(Json.toJson(Map(
          "status" -> "ok",
          "auth" -> authDetails.auth,
          "apisecret" -> authDetails.apiSecret)))
      })
  }

  def createAccount = Action { implicit request =>

    loginForm.bindFromRequest.fold(
      formWithErrors => errResult("Username and password are two required fields."),

      user =>
        if (user.password.length < 8) // TODO put that in a fucking conf
          errResult("Password is too short. Min length:8")
        else {
          Users.createUser(user, request.remoteAddress) match {
            case Right(errmsg) => errResult(errmsg)
            case Left(authToken) => Ok(Json.toJson(Map(
              "status" -> "ok",
              "auth" -> authToken.token)))
          }
        })
  }

  def logout = Action {
    BadRequest
  }

  private def errResult(msg: String) = Ok(Json.toJson(Map("status" -> "err", "error" -> msg)))

}
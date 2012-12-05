package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._

import models._

object Application extends Controller with Secured {

  def index = IsMaybeAuthenticated {
    user =>
      request => {
        val freshNews = List(
          News(1l, "Apple are better than pears", "http://lemonde.fr"),
          News(2l, "Secutix may sell you tix", "http://secutix.com"))

        Ok(views.html.index(user, "Your new application is ready.", freshNews))
      }
  }

  def login = Action {
    Ok(views.html.login())
  }

  def logout(apisecret: String) = IsMaybeAuthenticated {
    user =>
      request => user.map {
        u =>
          if (u.apisecret == apisecret) Users.updateAuthToken(u)
          Redirect("/")
      } getOrElse (Forbidden)
  }

  def userProfile(username: String) = IsMaybeAuthenticated {
    user =>
      request => {
        Users.getUserByUsername(username).map {
          visitedUser =>
            val activity = Users.getUserActivity(visitedUser)
            Ok(views.html.user(user, visitedUser, activity, gravatarUrl(visitedUser)))
        } getOrElse (Forbidden)
      }
  }

  import java.security.MessageDigest

  private def gravatarUrl(user: User) = {
    val md5 = MessageDigest.getInstance("MD5")
    val digest = md5.digest(user.email.getBytes).map("%02x".format(_)).mkString
    "http://gravatar.com/avatar/" + digest + "?s=48&d=mm"
  }

}

trait Secured {

  /**
   * Retrieve the authentication token.
   */
  private def authToken(request: RequestHeader): Option[AuthToken] = request.cookies.get("auth").map(a => AuthToken(a.value))

  /**
   * Retrieve the authenticated user.
   */
  private def getUser(request: RequestHeader): Option[User] = authToken(request) flatMap (a => Users.authUser(a))

  def Authenticated[A](action: Option[User] => Action[A]): Action[(Action[A], A)] = {
    val authenticatedBodyParser = BodyParser { request =>
      val user = getUser(request)
      val innerAction = action(user)
      innerAction.parser(request).mapDone { body =>
        body.right.map(innerBody => (innerAction, innerBody))
      }
    }

    Action(authenticatedBodyParser) { request =>
      val (innerAction, innerBody) = request.body
      innerAction(request.map(_ => innerBody))
    }
  }

  //  /**
  //   * Redirect to login if the user in not authorized.
  //   */
  //  private def onUnauthorized = Results.Redirect(routes.Application.login)
  //  def IsAuthenticated(f: => User => Request[AnyContent] => Result) = {
  //    Authenticated(user => Action(request => user match {
  //      case Some(u) => f(u)(request)
  //      case None => onUnauthorized
  //    }))
  //  }

  def IsMaybeAuthenticated(f: => Option[User] => Request[AnyContent] => Result) = {
    Authenticated(user => Action(request => f(user)(request)))
  }

}
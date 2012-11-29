package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._

import models._


object Application extends Controller with Secured {

  def index =
    /*Action {
          val freshNews = List(
            News(1l, "Apple are better than pears", "http://lemonde.fr"),
            News(2l, "Secutix may sell you tix", "http://secutix.com"))

          Ok(views.html.index(Some(User("idx", "y", "z")), "Your new application is ready.", freshNews))
        }
        */

    MaybeAuthenticated {
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

  def logout(apisecret: String) =

    MaybeAuthenticated {
      user =>
        request =>
          user map {
            u =>
              {
                if (u.apisecret == apisecret) {
                  Users.updateAuthToken(u)
                }
                Redirect("/")
              }
          } getOrElse (Forbidden)
    }

}

trait Secured {

  /**
   * Retrieve the authentication token.
   */
  private def authToken(request: RequestHeader): Option[AuthToken] = request.cookies.get("auth").map(a => AuthToken(a.value))

  private def getUser(request: RequestHeader) = authToken(request) flatMap (a => Users.authUser(a))

  //  def Authenticated[A](
  //    username: RequestHeader => Option[String],
  //    onUnauthorized: RequestHeader => Result)(action: String => Action[A]): Action[(Action[A], A)] = {
  //
  //    val authenticatedBodyParser = BodyParser { request =>
  //      username(request).map { user =>
  //        val innerAction = action(user)
  //        innerAction.parser(request).mapDone { body =>
  //          body.right.map(innerBody => (innerAction, innerBody))
  //        }
  //      }.getOrElse {
  //        Done(Left(onUnauthorized(request)), Input.Empty)
  //      }
  //    }
  //
  //    Action(authenticatedBodyParser) { request =>
  //      val (innerAction, innerBody) = request.body
  //      innerAction(request.map(_ => innerBody))
  //    }
  //  }

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

  def MaybeAuthenticated(f: => Option[User] => Request[AnyContent] => Result) = {
    Authenticated(user => Action(request => f(user)(request)))
  }

}

//
//trait Secured {
//
//  /**
//   * Redirect to login if the user in not authorized.
//   */
//  private def onUnauthorized(request: RequestHeader) = Results.Redirect(routes.Application.login)
//
//  /**
//   * Action for authenticated users.
//   */
//  def IsAuthenticated(f: => String => Request[AnyContent] => Result) = Security.Authenticated(authToken, onUnauthorized) { 
//		user =>  Action(request => f(user)(request))
//  }
//}


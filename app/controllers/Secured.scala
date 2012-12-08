package controllers

import play.api.mvc._
import models._

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

	/**
	 * Redirect to login if the user in not authorized.
	 */
	private def onUnauthorized = Results.Redirect(routes.Application.login)

	def IsAuthenticated(f: => User => Request[AnyContent] => Result) = {
		Authenticated(user => Action(request => user match {
			case Some(u) => f(u)(request)
			case None => onUnauthorized
		}))
	}

	def IsMaybeAuthenticated(f: => Option[User] => Request[AnyContent] => Result) = {
		Authenticated(user => Action(request => f(user)(request)))
	}

}
package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._
import models._
import util.GravatarUrl

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
						Ok(views.html.user(user, visitedUser, activity, GravatarUrl(visitedUser.email)))
				} getOrElse (Forbidden)
			}
	}

	def submit = IsAuthenticated {
		user => request => Ok(views.html.submit(user))
	}

}


package controllers

import play.api._
import play.api.mvc._
import models._

object Application extends Controller {

	def index = Action {

		val freshNews = List(
			News(1l, "Apple are better than pears", "http://lemonde.fr"), News(2l, "Secutix may sell you tix", "http://secutix.com"))

		Ok(views.html.index("Your new application is ready.", freshNews))
	}

}
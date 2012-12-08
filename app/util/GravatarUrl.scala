package util

import java.security.MessageDigest

object GravatarUrl {
	def apply(email: String): String = {
		val md5 = MessageDigest.getInstance("MD5")
		val digest = md5.digest(email.getBytes).map("%02x".format(_)).mkString
		"http://gravatar.com/avatar/" + digest + "?s=48&d=mm"
	}
}
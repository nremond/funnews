package models

import com.redis._

import security.PBKDF2

case class User(id: String, username: String, email: String, apisecret: String, karma: String, auth: AuthToken)
case class AuthToken(token: String)

object Users {

  val r = new RedisClient("localhost", 6379)

  /**
   * Return the user from the ID.
   */
  def getUserById(id: Int) = r.hgetall("user:" + id)

  /**
   * Return the user from the username.
   */
  def getUserByUsername(username: String) =
    r.get(usernameToIdKey(username)) flatMap (id => getUserById(id.toInt))

  case class AuthenticationDetails(auth: String, apiSecret: String)

  /**
   * Check if the username/password pair identifies an user.
   * If so the auth token and form secret are returned, otherwise nil is returned.
   */
  def checkUserCredentials(username: String, password: String): Option[AuthenticationDetails] =
    getUserByUsername(username) flatMap { user =>
      val hash = secureHash(password, user("salt"))
      if (hash == user("password"))
        Some(AuthenticationDetails(user("auth"), user("apisecret")))
      else
        None
    }

  case class Login(username: String, password: String)

  def createUser(login: Login, remoteAddress: String): Either[AuthToken, String] = {

    if (r.exists(usernameToIdKey(login.username)))
      Right("This username is already taken, please try a different one.")
    else if (rateLimitByIP(1, "create_user", remoteAddress)) //TODO 15->3600*15
      Right("Please wait some time before creating a new user.")
    else {
      val id = r.incr("users.count").get
      val authToken = randHexString
      val salt = randHexString
      val currentTimeSec = System.currentTimeMillis / 1000
      r.hmset("user:" + id,
        Map(
          "id" -> id,
          "username" -> login.username,
          "salt" -> salt,
          "password" -> secureHash(login.password, salt),
          "ctime" -> currentTimeSec,
          "karma" -> "TODO config shit",
          "about" -> "",
          "email" -> "",
          "auth" -> authToken,
          "apisecret" -> randHexString,
          "flags" -> "",
          "karma_incr_time" -> currentTimeSec))

      r.set(usernameToIdKey(login.username), id)
      r.set("auth:" + authToken, id)

      // First user ever created (id = 1) is an admin
      if (id == 1)
        r.hmset("user:" + id, Map("flags" -> "a"))

      Left(AuthToken(authToken))
    }
  }

  /**
   *  Try to authenticate the user
   */
  def authUser(auth: AuthToken) = {
    r.get("auth:" + auth.token) flatMap (id => r.hgetall("user:" + id)) map (
      u => User(u("id"), u("username"), u("email"), u("apisecret"), u("karma"), AuthToken(u("auth"))))
  }

  /**
   * Update the specified user authentication token with a random generated
   * one. This in other words means to logout all the sessions open for that
   * user.
   *
   * Return value: on success the new token is returned. Otherwise nil.
   * Side effect: the auth token is modified.
   */
  def updateAuthToken(user: User): AuthToken = {
    r.del("auth:" + user.auth.token)
    val newAuthToken = randHexString
    r.hmset("user:" + user.id, Map("auth" -> newAuthToken))
    r.set("auth:" + newAuthToken, user.id)
    AuthToken(newAuthToken)
  }

  /////////////////////

  //TODO 1000=PBKDF2Iterations to put in condif
  private def secureHash(password: String, salt: String) = PBKDF2(password, salt, 1000, 160 / 8)

  private def usernameToIdKey(username: String) = "username.to.id:" + username.toLowerCase()

  // Generic API limiting function
  private def rateLimitByIP(delay: Int, tags: String*) = {
    val key = "limit:" + tags.mkString(".")
    if (r.exists(key))
      true
    else {
      r.setex(key, delay, 1) // TODO what about the value of setex()  ?
      false
    }
  }

  /////TODO : were to put that

  def randHexString: String = {
    val a = new Array[Byte](20) // TODO config the 40/2=20 thing ?
    scala.util.Random.nextBytes(a)
    BigInt(1, a).toString(16)
  }

}
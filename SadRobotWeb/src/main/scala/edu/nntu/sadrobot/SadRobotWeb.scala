package edu.nntu.sadrobot

import unfiltered.request._
import unfiltered.response._
import unfiltered.scalate.Scalate
import unfiltered.formauth._

/**
 * Перекодировщик URL для нормальной работы с UTF-8
 * Отсюда: http://stackoverflow.com/questions/18083311/url-decoding-with-unfiltered
 */
object Decode {
  import java.net.URLDecoder
  import java.nio.charset.Charset

  trait Extract {
    def charset: Charset
    def unapply(raw: String) =
      scala.util.Try(URLDecoder.decode(raw, charset.name())).toOption
  }

  object utf8 extends Extract {
    val charset = Charset.forName("utf8")
  }
}

/**
 * http://www.slideshare.net/strachaj/introducing-scalate-the-scala-template-engine
 * http://scalate.github.io/scalate/documentation/ssp-reference.html
 * 
 * Scalate+Unfiltered:
 * https://github.com/unfiltered/unfiltered-scalate.g8/blob/master/src/main/g8/src/main/scala/Example.scala
 * 
 */
object SadRobotWeb {
  
  val securityCheck = JSecurityCheck(
    new Users() {
      def authenticate(username: String, password: String) = {
        username match {
          case "admin" if(password == "adminpw") => Some(User("admin", "admin", "adminpw"))
          case "anton" if(password == "user1pw") => Some(User("anton", "author", "user1pw"))
          case "ivan" if(password == "user1pw") => Some(User("ivan", "author", "user1pw"))
          case _ => None
        }
      }
    },
    (req: HttpRequest[javax.servlet.http.HttpServletRequest]) => {
      req match {
        case Path(Seg("admin" :: _)) => Some(Set("admin"))
        case Path(Seg("author" :: _)) => Some(Set("admin", "author"))
        case _ => None
      }
    },
    (req: HttpRequest[javax.servlet.http.HttpServletRequest], event: String) => {
      import unfiltered.scalate.Scalate
      
      event match {
        case "login" =>
          Unauthorized ~> Scalate(req, "auth/login.ssp")
        case "error" => 
          Unauthorized ~> Scalate(req, "auth/error.ssp")
        case "forbidden" => 
          Forbidden ~> Scalate(req, "auth/forbidden.ssp")
      }
    }
  )

  def main(args: Array[String]) {
//    println( new java.io.File("hello").getAbsolutePath )
//    Model.fillTestData
//    println("all robots:")
//    for(robot <- Model.robots()) println(robot.name)
//    
//    println("author:" + Model.author("anton").get.login)
//    
//    println("robots:")
//    for(robot <- Model.robots(Model.author("anton").get)) println(robot.name)
    
    
    println("Starting Scala Unfiltered web application (on jetty http server)...")
    println("Resources dir: " + getClass.getResource("/public"))

    // Запустить веб-сервер
    unfiltered.jetty.Server.http(8080).resources(getClass.getResource("/public"))
    .plan(securityCheck).plan(PublicArea).plan(AuthorArea).plan(AdminArea).plan(ApiArea).run()
  }
}

package edu.nntu.sadrobot

import unfiltered.request._
import unfiltered.response._
import unfiltered.scalate.Scalate

/**
 * Зона автора /author/xxx
 */
object AuthorArea extends unfiltered.filter.Plan {

  import SadRobotWeb._
  
  def currentAuthor[T](req: HttpRequest[T]) = {
    // т.к. мы в защищенной области сайта, пользователь должен быть полюбому
    val user = securityCheck.remoteUser(req).get
    Model.author(user.name).get
  }
  
  /**
   * Найти робота по идентификатору id, полученному из строки URL,
   * поэтому делается дополнительная проверка на то, что он представлял собой
   * корректной число Long
   */
  def robotById(id: String, onlyPublished: Boolean = true) = {
    try {
      Model.robot(id.toLong, onlyPublished)
    } catch {
      case e: Exception => None
    }
  }
    
  def intent = {
    // зона автора - список роботов
    case req@GET(Path(Seg("author" :: Nil))) =>
      val author = currentAuthor(req)
      Ok ~> Scalate(req, "author/author_home.ssp", "author" -> author)
      
    case req@GET(Path(Seg("author" :: "robots" :: Nil))) =>
      val author = currentAuthor(req)
      Ok ~> Scalate(req, "author/author_home.ssp", "author" -> author)
      
      // добавить робота - экран добавления
    case req@GET(Path(Seg("author" :: "robots" :: "new" :: Nil))) =>
      val author = currentAuthor(req)
      val robot = Model.Robot(name = "Новый робот", author = author, photo1 = "/img/buggy.png")
      Ok ~> Scalate(req, "author/robot_create.ssp", "author" -> author, "robot" -> robot)
      
      // добавляем робота
    case req@POST( Path(Seg("author" :: "robots" :: "new" :: Nil)) & Params(params) ) =>
      if(params.contains("create")) {
        val author = currentAuthor(req)
        val robot = Model.robotFromParams(author, params)
        
        try {
          val id = Model.createRobot(robot).id
        
          Redirect("/author/robots/" + id)
        } catch {
          case e: Exception => 
            // что-то пошло не так, вернемся в режим редактирования
            // с сообщением об ошибке
            Ok ~> Scalate(req, "author/robot_create.ssp", 
                          "author" -> author, 
                          "robot" -> robot, 
                          "problem" -> Some("что-то пошло не так"))
        }
      } else
        Redirect("/author/")
      
      // информация о роботе
    case req@GET(Path(Seg("author" :: "robots" :: id :: Nil))) =>
      val author = currentAuthor(req)
      val robot = robotById(id, false)
      
      if(!robot.isEmpty && author.username == robot.get.author.username)
        Ok ~> Scalate(req, "author/robot_info.ssp", "robot" -> robot.get)
      else
        Pass
      
      // редактировать робота - экран редактирования
    case req@GET(Path(Seg("author" :: "robots" :: id :: "edit" :: Nil))) =>
      val author = currentAuthor(req)
      val robot = robotById(id, false)
      
      if(!robot.isEmpty && author.username == robot.get.author.username)
        Ok ~> Scalate(req, "author/robot_edit.ssp", "author" -> author, "robot" -> robot.get)
      else 
        Pass
      
      // сохраняем изменения
    case req@POST( Path(Seg("author" :: "robots" :: id :: "edit" :: Nil)) & Params(params) ) =>
  
      val author = currentAuthor(req)
      val robot = robotById(id, false)
      
      if(!robot.isEmpty && author.username == robot.get.author.username) {
        if(params.contains("update_info")) {
          try {
            Model.updateRobotInfo(
              robot = robot.get,
              name = params("name").head,
              sid = if(params.contains("sid") && !params("sid").head.isEmpty) Some(params("sid").head) else None,
              photo1 = params("photo1").head, 
              otherPhotos = Some(params("otherPhotos")),
              video1 = Some(params("video1").head),
              otherVideos = Some(params("otherVideos")),
              description = Some(params("description").head),
              tags = Seq(params("tags").head),
              moreInfoUrl = scala.util.Try(java.net.URLDecoder.decode(params("moreInfoUrl").head, "utf8")).toOption,
              sourceCodeUrl = scala.util.Try(java.net.URLDecoder.decode(params("sourceCodeUrl").head, "utf8")).toOption,
              priceBtc = Some(params("priceBtc").head.toDouble),
              priceRur = Some(params("priceRur").head.toDouble),
              priceUsd = Some(params("priceUsd").head.toDouble) )
          
            Redirect("/author/robots/" + id)
          } catch {
            case e: Exception => 
              // что-то пошло не так, вернемся в режим редактирования
              // с сообщением об ошибке
              val newRobot = Model.robotFromParams(author, params)
              Ok ~> Scalate(req, "author/robot_edit.ssp", 
                            "author" -> author, 
                            "robot" -> newRobot, 
                            "problem" -> Some("что-то пошло не так"))
          }
        } else if(params.contains("publish")) {
          // публикуем робота
          if(params.contains("confirmed") && params("confirmed").head == "true" ) {
            Model.updateRobotPublishedStatus(robot.get, true)
            Redirect("/author/robots/" + id)
          } else {
            // показать экран с запросом подтверждения
            Ok ~> Scalate( req, "common/confirm_action.ssp", 
                          "caption" -> "Опубликовать робота?", 
                          "form_action" -> "",
                          "submit_name" -> "publish",
                          "submit_value" -> "Опубликовать",
                          // убрать имя кнопки с действием
                          // из списка исходных параметров
                          "params" -> (params - "publish")
            )
          }
        } else if(params.contains("unpublish")) {
          // отменяем публикацию робота
          if(params.contains("confirmed") && params("confirmed").head == "true" ) {
            Model.updateRobotPublishedStatus(robot.get, false)
            Redirect("/author/robots/" + id)
          } else {
            // показать экран с запросом подтверждения
            Ok ~> Scalate( req, "common/confirm_action.ssp", 
                          "caption" -> "Отменить публикацию робота?", 
                          "form_action" -> "",
                          "submit_name" -> "unpublish",
                          "submit_value" -> "Отменить публикацию",
                          // убрать имя кнопки с действием
                          // из списка исходных параметров
                          "params" -> (params - "unpublish")
            )
          }
        } else if(params.contains("delete")) {
          // удаляем робота
          if(params.contains("confirmed") && params("confirmed").head == "true" ) {
            Model.deleteRobot(robot.get)
            Redirect("/author/")
          } else {
            // показать экран с запросом подтверждения
            Ok ~> Scalate( req, "common/confirm_action.ssp", 
                          "caption" -> "Удалить робота?", 
                          "form_action" -> "",
                          "submit_name" -> "delete",
                          "submit_value" -> "Удалить",
                          // убрать имя кнопки с действием
                          // из списка исходных параметров
                          "params" -> (params - "delete")
            )
          }
        } else {
          Redirect("/author/robots/" + id) 
        }
      } else
        Pass
      
      // страницы робота
    case req@GET(Path(Seg("author" :: "robots" :: id :: "pages" :: Nil))) =>
      val author = currentAuthor(req)
      val robot = robotById(id, false)
      
      if(!robot.isEmpty && author.username == robot.get.author.username)
        Ok ~> Scalate(req, "author/robot_pages.ssp", "robot" -> robot.get)
      else
        Pass
      
      // редактировать страницу
    case req@GET(Path(Seg("author" :: "robots" :: id :: "pages" :: page :: "edit" :: Nil))) =>
      val author = currentAuthor(req)
      val robot = robotById(id, false)
      
      if(!robot.isEmpty && author.username == robot.get.author.username) {
        val persistedRobotPage = Model.robotPage(robot.get, page)
        val robotPage = 
          if(!persistedRobotPage.isEmpty) persistedRobotPage.get else
            Model.RobotPage(robot.get, page, "")
        
        Ok ~> Scalate(req, "author/robot_page_edit.ssp", 
                      "robot" -> robot.get, "page" -> robotPage)
      } else
        Pass
      
      // сохранить страницу
    case req@POST( Path(Seg("author" :: "robots" :: id :: "pages" :: page :: "edit" :: Nil)) 
                  & Params(params) ) =>
      val author = currentAuthor(req)
      val robot = robotById(id, false)
      
      if(!robot.isEmpty && author.username == robot.get.author.username) {
        val persistedRobotPage = Model.robotPage(robot.get, page)
        val robotPage = 
          if(!persistedRobotPage.isEmpty) persistedRobotPage.get else
            Model.RobotPage(robot.get, page, "")
        
        if(params.contains("save")) {
          try {
            // создать или сохранить страницу
            Model.updateRobotPage(
              robotPage = robotPage,
              content = params("content").head)
          
            Redirect("/author/robots/" + id + "/pages")
          } catch {
            case e: Exception => 
              // что-то пошло не так, вернемся в режим редактирования
              // с сообщением об ошибке
              val changedPage = Model.RobotPage(robot.get, page, params("content").head)
              Ok ~> Scalate(req, "author/robot_page_edit.ssp", 
                            "robot" -> robot.get, "page" -> changedPage)
          }
        } else if(params.contains("delete")) {
          // удаляем страницу робота
          if(params.contains("confirmed") && params("confirmed").head == "true" ) {
            Model.deleteRobotPage(robotPage)
            Redirect("/author/robots/" + id + "/pages")
          } else {
            // показать экран с запросом подтверждения
            Ok ~> Scalate( req, "common/confirm_action.ssp", 
                          "caption" -> "Удалить страницу?", 
                          "form_action" -> "",
                          "submit_name" -> "delete",
                          "submit_value" -> "Удалить",
                          // убрать имя кнопки с действием
                          // из списка исходных параметров
                          "params" -> (params - "delete")
            )
          }
        } else {
          Redirect("/author/robots/" + id + "/pages") 
        }
      } else
        Pass
  }
}

package edu.nntu.sadrobot

import unfiltered.request._
import unfiltered.response._
import unfiltered.scalate.Scalate

object PublicArea extends unfiltered.filter.Plan {
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
  
  /**
   * Ссылка на публичную страницу робота.
   */
  def robotUrl(robot: Model.Robot with sorm.Persisted) = {
    if(!robot.sid.isEmpty) "/r/" + robot.sid.get else "/robots/" + robot.id
  }
  
  def intent = {
    /*****************************/
    // Общая зона
    
    /****************************/
    // Роботы
    case req@GET(Path(Seg(Nil))) =>
      Ok ~> Scalate( req, "robots.ssp" )
    case req@GET(Path(Seg("robots" :: Nil))) =>
      Ok ~> Scalate( req, "robots.ssp" )
      
      // робот по идентификатору
    case req@GET(Path(Seg("robots" :: id :: Nil))) =>
      val robot = robotById(id)
      if(!robot.isEmpty)
        Ok ~> Scalate( req, "robot.ssp", "robot" -> robot.get )
      else 
        Pass
      
      // робот по строковому идентификатору
    case req@GET( Path(Decode.utf8(Seg("r" :: sid :: Nil))) ) =>
      val robot = Model.robot(sid, true)
      if(!robot.isEmpty)
        Ok ~> Scalate( req, "robot.ssp", "robot" -> robot.get )
      else 
        Pass
      
      // manual
    case req@GET(Path(Seg("robots" :: id :: "manual" :: Nil))) =>
      val robot = robotById(id)
      val page = Model.robotPage(robot.get, "manual")
      if(!robot.isEmpty)
        Ok ~> Scalate( req, "robot/robot_manual.ssp", 
                      "robot" -> robot.get, "page" -> page )
      else 
        Pass
      
    case req@GET( Path(Decode.utf8(Seg("r" :: sid :: "manual" :: Nil))) ) =>
      val robot = Model.robot(sid, true)
      val page = Model.robotPage(robot.get, "manual")
      if(!robot.isEmpty)
        Ok ~> Scalate( req, "robot/robot_manual.ssp", 
                      "robot" -> robot.get, "page" -> page )
      else 
        Pass
      
      // inaction
    case req@GET(Path(Seg("robots" :: id :: "inaction" :: Nil))) =>
      val robot = robotById(id)
      val page = Model.robotPage(robot.get, "inaction")
      if(!robot.isEmpty)
        Ok ~> Scalate( req, "robot/robot_inaction.ssp", 
                      "robot" -> robot.get, "page" -> page )
      else 
        Pass
      
    case req@GET( Path(Decode.utf8(Seg("r" :: sid :: "inaction" :: Nil))) ) =>
      val robot = Model.robot(sid, true)
      val page = Model.robotPage(robot.get, "inaction")
      if(!robot.isEmpty)
        Ok ~> Scalate( req, "robot/robot_inaction.ssp", 
                      "robot" -> robot.get, "page" -> page )
      else 
        Pass
      
      // source code
    case req@GET(Path(Seg("robots" :: id :: "src" :: Nil))) =>
      val robot = robotById(id)
      val page = Model.robotPage(robot.get, "src")
      if(!robot.isEmpty)
        Ok ~> Scalate( req, "robot/robot_src.ssp", 
                      "robot" -> robot.get, "page" -> page )
      else 
        Pass
      
    case req@GET( Path(Decode.utf8(Seg("r" :: sid :: "src" :: Nil))) ) =>
      val robot = Model.robot(sid, true)
      val page = Model.robotPage(robot.get, "src")
      if(!robot.isEmpty)
        Ok ~> Scalate( req, "robot/robot_src.ssp", 
                      "robot" -> robot.get, "page" -> page )
      else 
        Pass
      
      // gallery
    case req@GET(Path(Seg("robots" :: id :: "gallery" :: Nil))) =>
      val robot = robotById(id)
      val page = Model.robotPage(robot.get, "gallery")
      if(!robot.isEmpty)
        Ok ~> Scalate( req, "robot/robot_gallery.ssp", 
                      "robot" -> robot.get, "page" -> page )
      else 
        Pass
      
    case req@GET( Path(Decode.utf8(Seg("r" :: sid :: "gallery" :: Nil))) ) =>
      val robot = Model.robot(sid, true)
      val page = Model.robotPage(robot.get, "gallery")
      if(!robot.isEmpty)
        Ok ~> Scalate( req, "robot/robot_gallery.ssp", 
                      "robot" -> robot.get, "page" -> page )
      else
        Pass
      
      /****************************/
      // Авторы
    case req@GET(Path(Seg("authors" :: Nil))) =>
      Ok ~> Scalate( req, "authors.ssp" )
      
    case req@GET(Path(Seg("authors" :: authorUsername :: Nil))) =>
      val author = Model.author(authorUsername)
      if(!author.isEmpty)
        Ok ~> Scalate( req, "author.ssp", "author" -> author.get )
      else
        Pass
      
      /*****************************/
      // Корзина
    case req@GET(Path(Seg("cart" :: Nil))) =>
      Ok ~> Scalate( req, "cart.ssp",
                    "currency" -> CartManager.currency(req).get, 
                    "robots" -> CartManager.robots(req).get,
                    "order" -> CartManager.order(req).get )
      
    case req@POST( Path(Seg("cart" :: Nil)) & Params(params) ) =>
      val robots = Model.robotsFromParams(params)
      CartManager.addRobots(robots, req)
      Redirect("/cart/")
        
      // "виджет": список роботов в корзине
    case req@Path(Seg("cart" :: "robots" :: Nil)) =>
      val currency = CartManager.currency(req).get
      val robots = CartManager.robots(req).get
      Ok ~> Scalate( req, "common/cart_robots.ssp", 
                    "currency" -> currency, "robots" -> robots )
      
      // "виджет": статус корзины - иконка и количество роботов
    case req@Path(Seg("cart" :: "icon" :: Nil)) =>
      val count = CartManager.robots(req).get.size
      Ok ~> Scalate( req, "common/cart_icon.ssp", "count" -> count )
      
      /*****************************/
      // Заказ order
      // Просмотр заказа
    case req@POST( Path(Seg("order" :: Nil)) & Params(params) ) if(params.contains("review")) =>
      val order = Model.orderFromParams(params)
      // сохраним заказ в корзине
      CartManager.setOrder(order, req)
      Ok ~> Scalate( req, "order_review.ssp", "order" -> order )
      
      // Подтверждение заказа
    case req@POST( Path(Seg("order" :: Nil)) & Params(params) ) if(params.contains("confirm")) =>
      val order = Model.orderFromParams(params)
      val sid = Model.confirmOrder(order).sid.get
      // очистить корзину
      CartManager.clearRobots(req)
      // посмотреть сводку о заказе
      Redirect("/order/" + sid)
            
      // просмотреть заказ
    case req@Path(Seg("order" :: sid :: Nil)) =>
      val order = Model.orderBySid(sid)
      if(!order.isEmpty)
        Ok ~> Scalate( req, "order_status.ssp", "order" -> order.get )
      else
        Pass
  }
}

package edu.nntu.sadrobot

import unfiltered.request._
import unfiltered.response._
import unfiltered.scalate.Scalate

object ApiArea extends unfiltered.filter.Plan {
  
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
    // добавить робота в корзину
    case req@POST( Path(Seg("api" :: "cart" :: "add" :: Nil)) & Params(params) ) =>
      val id = params("id").head
      val quantity = params("quantity").head.toInt
      
      val robot = robotById(id)
      if(!robot.isEmpty) {
        CartManager.addRobot(robot.get, quantity.toInt, req)
        Ok ~> PlainTextContent ~> ResponseString("ok")
      } else
        Ok ~> PlainTextContent ~> ResponseString("robot id " + id + " not found")
      
      // обновить робота в корзине
    case req@POST( Path(Seg("api" :: "cart" :: "update" :: Nil)) & Params(params) ) =>
      val id = params("id").head
      val quantity = params("quantity").head.toInt
      
      val robot = robotById(id)
      if(!robot.isEmpty) {
        CartManager.updateRobot(robot.get, quantity.toInt, req)
        Ok ~> PlainTextContent ~> ResponseString("ok")
      } else
        Ok ~> PlainTextContent ~> ResponseString("robot id " + id + " not found")
      
      // удалить робота из корзины
    case req@POST( Path(Seg("api" :: "cart" :: "remove" :: Nil)) & Params(params) ) =>
      val id = params("id").head
      
      val robot = robotById(id)
      if(!robot.isEmpty) {
        CartManager.removeRobot(robot.get, req)
        Ok ~> PlainTextContent ~> ResponseString("ok")
      } else
        Ok ~> PlainTextContent ~> ResponseString("robot id " + id + " not found")
      
      // очистить корзину
    case req@POST( Path(Seg("api" :: "cart" :: "clear" :: Nil)) ) =>
      CartManager.clearRobots(req)
      Ok ~> PlainTextContent ~> ResponseString("ok")
  }
}

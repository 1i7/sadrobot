package edu.nntu.sadrobot

import unfiltered.request._

import javax.servlet.http.HttpServletRequest

object CartManager {
  import Model._
  import Model.Currency._
  
  /**
   * Корзина с покупками.
   */
  case class Cart(
    var currency: Currency = Currency.RUR,
    var robots: Map [Robot, (/*quantity:*/ Int, /*price:*/ Double)] = Map(),
    var order: RobotOrder = RobotOrder()
  )
  
  /**
   * Содержит корзины для открытых сессий
   */
  private var sessionCarts = Map[String, Cart]()
  
  /**
   * Get session id for the given HTTP request.
   * 
   * @param req
   * 
   * @return session id as Option[String] for the HTTP request or None if 
   * sessions are not supported with current application container
   */
  private def getSessionId[T](req : HttpRequest[T]) = {
    var sessionId : Option[String] = None
    if(req.underlying.isInstanceOf[javax.servlet.http.HttpServletRequest]) {
      val jreq = req.underlying.asInstanceOf[javax.servlet.http.HttpServletRequest]
      try {
        sessionId = Some(jreq.getSession.getId)
      } catch {
        case e : Throwable => e.printStackTrace
      }
    }
    sessionId
  }
  
  /**
   * Получить корзину для текущей сессии.
   */
  def cart[T](req : HttpRequest[T]) = {
    val sessionId = getSessionId(req)
    if(!sessionId.isEmpty) {
      if(!sessionCarts.contains(sessionId.get)) {
        sessionCarts += (sessionId.get -> Cart())
      }
      Some(sessionCarts(sessionId.get))
    } else {
      None
    }
  }
  
  /**
   * Валюта корзины.
   */
  def currency[T](req : HttpRequest[T])  = {
    val cart = this.cart(req)
    if(!cart.isEmpty) {
      Some(cart.get.currency)
    } else {
      None
    }
  }
  
  /**
   * Получить роботов из корзины.
   */
  def robots[T](req : HttpRequest[T])  = {
    val cart = this.cart(req)
    if(!cart.isEmpty) {
      Some(cart.get.robots)
    } else {
      None
    }
  }
  
  /**
   * Получить заказ из корзины.
   */
  def order[T](req : HttpRequest[T]) = {
    val cart = this.cart(req)
    if(!cart.isEmpty) {
      Some(cart.get.order)
    } else {
      None
    }
  }
  
  /**
   * Установить валюту корзины.
   */
  def setCurrency[T](currency: Currency, req : HttpRequest[T]) {
    val cart = this.cart(req)
    if(!cart.isEmpty) {
      cart.get.currency = currency
      
      // обновим цены для уже добавленных роботов
      for((robot, (quantity, price)) <- cart.get.robots) {
        // цену робота берем из базы
        val newPrice = cart.get.currency match {
          case BTC => robot.priceBtc
          case RUR => robot.priceRur
          case USD => robot.priceUsd
        }
        cart.get.robots += ( robot -> ((quantity, newPrice.get)) )
      }
      
      // обновим заказ
      cart.get.order = cart.get.order.copy(currency = currency, robots = cart.get.robots)
    }
  }
  
  /**
   * Добавить робота в корзину.
   */
  def addRobot[T](robot: Robot, quantity: Int, req : HttpRequest[T]) {
    val cart = this.cart(req)
    if(!cart.isEmpty) {
      // цену робота берем из базы
      val price = cart.get.currency match {
        case BTC => robot.priceBtc
        case RUR => robot.priceRur
        case USD => robot.priceUsd
      }
      cart.get.robots += ( robot -> ((quantity, price.get)) )
      
      // обновим заказ
      cart.get.order = cart.get.order.copy(robots = cart.get.robots)
    }
  }
  
  /**
   * Добавить роботов в корзину.
   */
  def addRobots[T](robots: Map[Robot, Int], req : HttpRequest[T]) {
    val cart = this.cart(req)
    if(!cart.isEmpty) {
      for((robot, quantity) <- robots) {
        // цену робота берем из базы
        val price = cart.get.currency match {
          case BTC => robot.priceBtc
          case RUR => robot.priceRur
          case USD => robot.priceUsd
        }
        cart.get.robots += ( robot -> ((quantity, price.get)) )
      }
      
      // обновим заказ
      cart.get.order = cart.get.order.copy(robots = cart.get.robots)
    }
  }
  
  /**
   * Обновить робота в корзине.
   */
  def updateRobot[T](robot: Robot, quantity: Int, req : HttpRequest[T]) {
    val cart = this.cart(req)
    if(!cart.isEmpty) {
      // цену робота берем из базы
      val price = cart.get.currency match {
        case BTC => robot.priceBtc
        case RUR => robot.priceRur
        case USD => robot.priceUsd
      }
      cart.get.robots += ( robot -> ((quantity, price.get)) )
      
      // обновим заказ
      cart.get.order = cart.get.order.copy(robots = cart.get.robots)
    }
  }
  
  /**
   * Убрать робота из корзины.
   */  
  def removeRobot[T](robot: Robot, req : HttpRequest[T]) {
    val cart = this.cart(req)
    if(!cart.isEmpty) {
      cart.get.robots -= robot
      
      // обновим заказ
      cart.get.order = cart.get.order.copy(robots = cart.get.robots)
    }
  }
  
  /**
   * Удалить всех роботов из корзины.
   */
  def clearRobots[T](req : HttpRequest[T]) {
    val cart = this.cart(req)
    if(!cart.isEmpty) {
      cart.get.robots = Map()
      
      // обновим заказ
      cart.get.order = cart.get.order.copy(robots = cart.get.robots)
    }
  }
  
  /**
   * Проверить, находится ли робот в корзине.
   */
  def robotInCart[T](req : HttpRequest[T], robot: Robot) = {
    val cart = this.cart(req)
    if(!cart.isEmpty) {
      cart.get.robots.contains(robot)
    } else {
      false
    }
  }
 
   
  
  /**
   * Обновить информацию о заказе в корзине.
   */
  def setOrder[T](order: RobotOrder, req : HttpRequest[T]) {
    val cart = this.cart(req)
    if(!cart.isEmpty) {
      // обновим валюту и роботов из заказа
      cart.get.currency = order.currency
      cart.get.robots = order.robots
      
      cart.get.order = order
    }
  }
}

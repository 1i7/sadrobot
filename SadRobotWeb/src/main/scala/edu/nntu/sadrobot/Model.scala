
package edu.nntu.sadrobot

/**
 * http://sorm-framework.org/Documentation.html
 */
object Model {
  
  // Declare a model:
  case class Author ( 
    username: String,
    displayName: String,
    email: String )
  
  object RobotStatus extends Enumeration {
    type RobotStatus = Value
    val InStock, OnDemand, NotAvailable, Discontinued = Value
  }
  
  import RobotStatus._
  
  case class Robot (
    // basic info
    name: String,
    author: Author,
    sid: Option[String] = None,
    
    createTime: Option[org.joda.time.DateTime] = None,
    updateTime: Option[org.joda.time.DateTime] = None,
    
    // visuals
    photo1: String,
    otherPhotos: Option[Seq[String]] = None,
    video1: Option[String] = None,
    otherVideos: Option[Seq[String]] = None,
    
    // additional info
    description: Option[String] = None,
    tags: Seq[String] = Seq.empty,
    
    moreInfoUrl: Option[String] = None,
    sourceCodeUrl: Option[String] = None,
    
    // TODO:
//    links: Map[String, String] = Map(),
    
    // pricing
    // TODO:
//    prices: Map[Currency, Double] = Map(),
    priceBtc: Option[Double] = None,
    priceRur: Option[Double] = None,
    priceUsd: Option[Double] = None,
    
    // status
    status: RobotStatus = InStock,
    inStock: Int = 0,
    published: Boolean = false)
  
  
  /**
   * Pages with additional info (may contain at least 
   * "manual", "gallery", "inaction", "sourceCode")
   */
  case class RobotPage (
    robot: Robot,
    name: String,
    content: String)
  
  object Currency extends Enumeration {
    type Currency = Value
    val BTC, RUR, USD = Value
  }
  
  /**
   * Последовательные статусы заказа:
   * 0) Заказ не подтвержден (не внесен в базу данных)
   * 1) Заказ подвержден, ожидает выставления счета
   * 2) Счет выставлен, ожидает оплаты
   * 3) Счет оплачен, ожидает отправки
   * 4) Заказ отправлен, ожидает получения
   * 5) Заказ выполнен
   * 
   * отдельный статус:
   * 6) Заказ отменен
   */
  object OrderStatus extends Enumeration {
    type OrderStatus = Value
    val NA, Confirmed, Invoice, Payed, Shipped, Complete, Canceled = Value
    
    /**
     * Определить следующий статус заказа в зависимости от текущего.
     */
    def nextStatus(status: OrderStatus) = {
      status match {
        case NA => NA
        case Confirmed => Invoice
        case Invoice => Payed
        case Payed => Shipped
        case Shipped => Complete
        case Complete => Complete
        case Canceled => Canceled
      }
    }
  }
  
  import Currency._
  import OrderStatus._
  
  /**
   * Заказ роботов. Все роботы должны быть от одного автора.
   * Все суммы указаны в одной валюте заказа.
   */
  case class RobotOrder (
    sid: Option[String] = None,
    
    confirmTime: Option[org.joda.time.DateTime] = None,
    invoiceTime: Option[org.joda.time.DateTime] = None,
    paymentTime: Option[org.joda.time.DateTime] = None,
    shipmentTime: Option[org.joda.time.DateTime] = None,
    completeTime: Option[org.joda.time.DateTime] = None,
    cancelTime: Option[org.joda.time.DateTime] = None,
    
    // Info
    author: Option[Author] = None,
    
    robots: Map [Robot, (/*quantity:*/ Int, /*price:*/ Double)] = Map(),
    shippingPrice: Option[Double] = None,
    discount: Option[Double] = None,
    totalPrice: Option[Double] = None,
    
    // Payment info
    currency: Currency = Currency.RUR,
    paymentAddressBtc: Option[String] = None,
    
    // User info
    userFullName: Option[String] = None,
    userEmail: Option[String] = None,
    userAddress: Option[String] = None,
    userComment: Option[String] = None,
    
    // Status
    // Confirmed, Invoice, Payed, Shipped, Complete, Canceled
    status: OrderStatus = NA,
    
    // Comments
    publicComment: Option[String] = None,
    comment: Option[String] = None )
  
  // Initialize SORM, automatically generating schema:
  import sorm._
  object Db extends Instance(
    entities = Set( 
      Entity[Author](unique = Set() + Seq("username") + Seq("email")),
      Entity[Robot](unique = Set() + Seq("sid"), indexed = Set() + Seq("sid")),
      Entity[RobotPage](),
      Entity[RobotOrder](unique = Set() + Seq("sid"), indexed = Set() + Seq("sid")) ),
//    http://www.h2database.com/html/features.html#database_url
//    url = "jdbc:h2:mem:test",
    url = "jdbc:h2:~/sadrobot-h2",
    user = "",
    password = "",
    initMode = InitMode.Create
//    initMode = InitMode.DropAllCreate
  )
  var i = 0
  def fillTestData() = {
    var success = true
    println("Filling test data...")
    try {
      // Store values in the db:
    
      println("authors...")
      val author1 = Db.save( Author("anton", "Anton", "ii1i7@yandex.ru") )
      val author2 = Db.save( Author("ivan", "Ivan", "sadrobot@yandex.ru") )
      println("robots...")
      i = i +1
      val robot1 = createRobot( Robot(name = "Робот Машинка" + i, 
                                      author = author1, 
                                      photo1 = "http://img-fotki.yandex.ru/get/6801/161653612.99/0_106b61_d8f7b420_XL.jpg", 
                                      description = Some("Робот Машинка самодельный"), 
                                      priceBtc = Some(.5),
                                      priceRur = Some(7999),
                                      priceUsd = Some(110) ))
      val robot2 = createRobot( Robot(name = "Робовер" + i, 
                                      author = author2, 
                                      photo1 = "http://img-fotki.yandex.ru/get/6700/161653612.9b/0_107ce7_b83ba05c_XL.jpg", 
                                      description = Some("Ровер луноходный"), 
                                      priceBtc = Some(.3),
                                      priceRur = Some(4800),
                                      priceUsd = Some(70) ))
    } catch {
      case e : Throwable => 
        success = false
        println
        e.printStackTrace
    } finally {
      println("done")
    }
    success
  }
  
  /**
   * Все авторы на сайте.
   */  
  def authors() = {
    Db.query[Author].fetch()
  }
  
  /**
   * Найти автора по имени пользователя
   */
  def author(username : String) = {
    Db.query[Author].whereEqual("username", username).fetchOne()
  }
  
  /**
   * Все роботы на сайте.
   */
  def robots(onlyPublished: Boolean = true) = {
    if(onlyPublished) {
      Db.query[Robot].whereEqual("published", true).fetch()
    } else {
      Db.query[Robot].fetch()      
    }
  }
  
  /**
   * Роботы по автору.
   */
  def robots(author : Author, onlyPublished: Boolean) = {
    if(onlyPublished) {
      Db.query[Robot].whereEqual("author", author).whereEqual("published", true).fetch()
    } else {
      Db.query[Robot].whereEqual("author", author).fetch()
    }
  }
  
  /**
   * Робот по индентификатору.
   */
  def robot(id : Long, onlyPublished: Boolean = true) = {
    try {
      val robot = Db.fetchById[Robot](id)
      if(onlyPublished && !robot.published) {
        None
      } else {
        Some(robot)
      }
    } catch {
      case _: Throwable => None
    }
  }
  
  /**
   * Робот по строковому индентификатору.
   */
  def robot(sid : String, onlyPublished: Boolean) = {
    if(onlyPublished) {
      Db.query[Robot].whereEqual("sid", Some(sid)).whereEqual("published", true).fetchOne()
    } else {
      Db.query[Robot].whereEqual("sid", Some(sid)).fetchOne()
    }
  }
  
  /**
   * Сконструировать робота из параметров.
   */
  def robotFromParams(author: Author, params: Map[String, Seq[String]]) = {
    Robot(
      author = author,
      name = if(params.contains("name")) params("name").head else "",
      sid = if(params.contains("sid")) Some(params("sid").head) else None,
      photo1 = if(params.contains("photo1")) params("photo1").head else "",
      otherPhotos = if(params.contains("otherPhotos")) Some(params("otherPhotos")) else None, 
      video1 = if(params.contains("video1")) Some(params("video1").head) else None,
      otherVideos = if(params.contains("otherVideos")) Some(params("otherVideos")) else None,
      description = if(params.contains("description")) Some(params("description").head) else None,
      tags = if(params.contains("tags")) Seq(params("tags").head) else Seq.empty,
      moreInfoUrl = if(params.contains("moreInfoUrl")) 
        scala.util.Try(java.net.URLDecoder.decode(params("moreInfoUrl").head, "utf8")).toOption 
      else None,
      sourceCodeUrl = if(params.contains("sourceCodeUrl")) 
        scala.util.Try(java.net.URLDecoder.decode(params("sourceCodeUrl").head, "utf8")).toOption 
      else None,
      priceBtc = if(params.contains("priceBtc")) Some(params("priceBtc").head.toDouble) else None,
      priceRur = if(params.contains("priceRur")) Some(params("priceRur").head.toDouble) else None,
      priceUsd = if(params.contains("priceUsd")) Some(params("priceUsd").head.toDouble) else None
    )
  }
  
  /**
   * Сконструировать список роботов для заказа из параметров.
   * 
   * Последовательность параметров для робота, должна содержать подряд пары
   * значений [id,количество]:[Long,Int]: id1,quant1,id2,quant2,...
   */
  def robotsFromParams(params: Map[String, Seq[String]]) = {
    var robots = Map [Robot, Int] ()
    
    val robotParams = params("robot")
    val iter = robotParams.iterator
    while(iter.hasNext) {
      val robotId = iter.next.toLong
      val robotQuantity = iter.next.toInt
      
      // найдем робота в базе
      val robot = this.robot(robotId).get
      robots += ( robot -> robotQuantity )
    }
    robots
  }
  
  /**
   * Создать нового робота.
   */
  def createRobot(robot: Robot) = {
    // время создания робота
    val timestamp = org.joda.time.DateTime.now
    Db.save( robot.copy(
        createTime = Some(timestamp),
        updateTime = Some(timestamp)) )
  }
  
  /**
   * Удалить робота.
   */
  def deleteRobot(robot: Robot) = {
    Db.delete( robot )
  }
  
  /**
   * Обновить информацию о роботе.
   */
  def updateRobotInfo(
    robot: Robot,
    name: String,
    sid: Option[String],
    photo1: String,
    otherPhotos: Option[Seq[String]],
    video1: Option[String], 
    otherVideos: Option[Seq[String]],
    description: Option[String],
    tags: Seq[String],
    moreInfoUrl: Option[String],
    sourceCodeUrl: Option[String],
    priceBtc: Option[Double],
    priceRur: Option[Double],
    priceUsd: Option[Double] ) = {
    
    val timestamp = org.joda.time.DateTime.now
        
    Db.save( robot.copy(
        updateTime = Some(timestamp),
        name = name,
        sid = sid,
        photo1 = photo1,
        otherPhotos = otherPhotos,
        video1 = video1,
        otherVideos = otherVideos,
        description = description,
        tags = tags,
        moreInfoUrl = moreInfoUrl,
        sourceCodeUrl = sourceCodeUrl,
        priceBtc = priceBtc,
        priceRur = priceRur,
        priceUsd = priceUsd) )
  }
  
  /**
   * Опубликовать робота на главной странице или отменить публикацию.
   */
  def updateRobotPublishedStatus(robot: Robot with Persisted, published: Boolean) = {
    Db.save( robot.copy(published = published) )
  }
  
  /**
   * Find page for robot by name.
   */
  def robotPage(robot: Robot, name: String) = {
    Db.query[RobotPage].whereEqual("robot", robot).whereEqual("name", name).fetchOne()
  }
  
  /**
   * Обновить страницу робота.
   */
  def updateRobotPage(robotPage: RobotPage, content: String) = {        
    Db.save( robotPage.copy(content = content) )
  }
  
  /**
   * Удалить страницу робота.
   */
  def deleteRobotPage(robotPage: RobotPage) = {
    Db.delete( robotPage )
  }

  /**
   * Все заказы.
   */
  def orders() = {
    Db.query[RobotOrder].fetch()
  }
  
  /**
   * Найти заказ по уникальному строковому идентефикатору.
   */
  def orderBySid(sid: String) = {
    Db.query[RobotOrder].whereEqual("sid", Some(sid)).fetchOne()
  }
  
  /**
   * Сконструировать заказ из параметров.
   */
  def orderFromParams(params: Map[String, Seq[String]]) = {
    val currency = if(params.contains("currency")) 
      Currency.withName(params("currency").head) 
    else 
      Currency.RUR
    
    var author: Option[Author] = None
    
    var robots = Map [Robot, (/*quantity:*/ Int, /*price:*/ Double)] ()
    // последовательность параметров для робота, должна содержать подряд пары
    // значений [id,количество]:[Long,Int]: id1,quant1,id2,quant2,...
    val robotParams = params("robot")
    val iter = robotParams.iterator
    while(iter.hasNext) {
      val robotId = iter.next.toLong
      val robotQuantity = iter.next.toInt
      
      // достанем актуальную информацию о роботе (в первую очередь, цену) из базы
      val robot = this.robot(robotId).get
      val robotPrice = currency match {
        case BTC => robot.priceBtc
        case RUR => robot.priceRur
        case USD => robot.priceUsd
      }
      
      // tuple inside map: https://issues.scala-lang.org/browse/SI-4153
      robots += ( robot -> ((robotQuantity, robotPrice.get)) )
      
      // Автор в заказе - автор робота
      author = Some(robot.author)
    }
    
    RobotOrder(
      currency = currency,
      author = author,
      robots = robots,
      userFullName = if(params.contains("userFullName")) Some(params("userFullName").head) else None, 
      userEmail = if(params.contains("userEmail")) Some(params("userEmail").head) else None,
      userAddress = if(params.contains("userAddress")) Some(params("userAddress").head) else None,
      userComment = if(params.contains("userComment")) Some(params("userComment").head) else None
    )
  }
  
  /**
   * Подтвердить заказ.
   */
  def confirmOrder(order: RobotOrder) = {
    // сгенерируем длинный уникальнй строковый идентификатор для заказа
    val sid = java.util.UUID.randomUUID().toString.replaceAll("-", "")
    // и обозначим время заказа
    val timestamp = org.joda.time.DateTime.now
    Model.Db.save( order.copy(
        sid = Some(sid), 
        confirmTime = Some(timestamp), 
        status = OrderStatus.Confirmed) )
  }
  
  /**
   * Обновить статус заказа.
   */
  def updateOrderStatus(
    order: RobotOrder with Persisted, status: OrderStatus) = {
    
    val timestamp = org.joda.time.DateTime.now
    
    status match {
      case Confirmed =>
        Model.Db.save( order.copy(
            status = status, confirmTime = Some(timestamp)) )
      case Invoice => 
        Model.Db.save( order.copy(
            status = status, invoiceTime = Some(timestamp)) )
      case Payed => 
        Model.Db.save( order.copy(
            status = status, paymentTime = Some(timestamp)) )
      case Shipped => 
        Model.Db.save( order.copy(
            status = status, shipmentTime = Some(timestamp)) )
      case Complete => 
        Model.Db.save( order.copy(
            status = status, completeTime = Some(timestamp)) )
      case Canceled =>
        Model.Db.save( order.copy(
            status = status, cancelTime = Some(timestamp)) )
      case _ =>
        Model.Db.save( order.copy(
            status = status) )
    }
  }
  
  /**
   * Обновить информацию о цене заказа.
   */
  def updateOrderPrice(
    order: RobotOrder with Persisted, shippingPrice: Double, 
    discount: Double) = {
    
    val robotsPrice = orderRobotsPrice(order.robots)
    val totalPrice = robotsPrice + shippingPrice - discount
    
    Model.Db.save( order.copy(
        shippingPrice = Some(shippingPrice),
        discount = Some(discount),
        totalPrice = Some(totalPrice)) )
  }
  
  /**
   * Обновить информацию об оплате заказа.
   */
  def updateOrderPaymentInfo(
    order: RobotOrder with Persisted, paymentAddressBtc: String) = {
    
    Model.Db.save( order.copy(
        paymentAddressBtc = Some(paymentAddressBtc)) )
  }
  
  /**
   * Обновить публичный комментарий к заказу.
   */
  def updateOrderPublicComment(
    order: RobotOrder with Persisted, comment: String) = {
        
    Model.Db.save( order.copy(publicComment = Some(comment)) )
  }
  
  /**
   * Обновить закрытый комментарий к заказу.
   */
  def updateOrderComment(
    order: RobotOrder with Persisted, comment: String) = {
        
    Model.Db.save( order.copy(comment = Some(comment)) )
  }
  
  /**
   * Общая цена за роботов в заказе (без стоимости доставки и скидки).
   */
  def orderRobotsPrice(robots: Map [Robot, (/*quantity:*/ Int, /*price:*/ Double)]) = {
    var total: Double = 0
    if(!robots.isEmpty) for( (robot, (quantity, price)) <- robots ) {
      total += price * quantity
    }
    total
  }
}

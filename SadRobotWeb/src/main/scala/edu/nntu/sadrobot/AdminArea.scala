package edu.nntu.sadrobot

import unfiltered.request._
import unfiltered.response._
import unfiltered.scalate.Scalate

object AdminArea extends unfiltered.filter.Plan {
  def intent = {
    /*****************************/
    // Зона администратора - служебные вызовы
    case req@GET(Path(Seg("admin" :: Nil))) =>
      Ok ~> Scalate(req, "admin/admin_home.ssp")
      
    case Path(Seg("admin" :: "create_db" :: Nil)) =>
      val msg = if (Model.fillTestData) "База пересоздана" else "Ошибка при создании базы"
      Ok ~> PlainTextContent ~> ResponseString(msg)
      
      // управление заказами
      // список заказов
    case req@Path(Seg("admin" :: "orders" :: Nil)) =>
      Ok ~> Scalate( req, "admin/orders.ssp" )
      
      // информация о заказе
    case req@GET( Path(Seg("admin" :: "order" :: id :: Nil)) ) =>
      val order = Model.orderBySid(id)
      if(!order.isEmpty)
        Ok ~> Scalate( req, "admin/order_update.ssp", "order" -> order.get )
      else
        Pass
      
      // обновить информацию о заказе
    case req@POST( Path(Seg("admin" :: "order" :: id :: Nil)) & Params(params) )  =>
      val order = Model.orderBySid(id)
      if(!order.isEmpty) {
        if(params.contains("update_status")) {
          // статус заказа
          if(params.contains("confirmed") && params("confirmed").head == "true" ) {
            Model.updateOrderStatus(
              order = order.get, 
              status = Model.OrderStatus.withName(params("status").head) )
          
            Redirect("/admin/order/" + id)
          } else {
            // показать экран с запросом подтверждения
            Ok ~> Scalate( req, "common/confirm_action.ssp", 
                          "caption" -> "Изменить статус заказа?", 
                          "form_action" -> "",
                          "submit_name" -> "update_status",
                          "submit_value" -> "Изменить",
                          // убрать имя кнопки с действием
                          // из списка исходных параметров
                          "params" -> (params - "update_status")
            )
          }
        } else if(params.contains("update_price")) {
          // цена заказа
          Model.updateOrderPrice(
            order = order.get, 
            shippingPrice = params("shippingPrice").head.toDouble,
            discount = params("discount").head.toDouble )
          
          Redirect("/admin/order/" + id)
        } else if(params.contains("update_payment")) {
          // информация об оплате
          Model.updateOrderPaymentInfo(
            order = order.get,
            paymentAddressBtc = params("paymentAddressBtc").head )
          
          Redirect("/admin/order/" + id)
        } else if(params.contains("update_public_comment")) {
          // комментарий для пользователя
          Model.updateOrderPublicComment(
            order = order.get, 
            comment = params("publicComment").head )
          
          Redirect("/admin/order/" + id)
        } else if(params.contains("update_comment")) {
          // закрытый комментарий
          Model.updateOrderComment(
            order = order.get, 
            comment = params("comment").head )
          
          Redirect("/admin/order/" + id)
        } else {
          Redirect("/admin/order/" + id)
        }
      } else
        Pass
  }
}
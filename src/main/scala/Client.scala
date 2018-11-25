// Описатели клиента биржи

import scala.collection.mutable
import Settings._

// класс и объект-компаньон Клиента биржи

object Client {

  // шапка для "таблицы" клиентов (удобна для просмотра при отладке)
  def header: String =
    ( sequrityTitle.map( _.formatted("%5s") ) )
      .mkString( s"Name\t", "\t", "")
}

case class Client( name: String, sequrities: mutable.Map[String,ValueType] ) {

  // ещё вспомогательный конструктор - для чтения из строки TSV
  protected def this( arr: Array[String] ) = {
    this( arr.head, mutable.Map() ++
      (sequrityTitle zip arr.toList.tail).toMap.mapValues(v=>new ValueType(v.toLong)) )
    // ^ Long промежуточный тип; (исправить в случае изменения на Double итп)
    assert( arr.size == sequrityTitle.length+1 )
  }
  def this( line: String ) = this( line.stripLineEnd.split("\t") ) // .getLines.map(_.stripLineEnd.split("\t", -1))

  // вывод в формате читаемого TSV
  override def toString: String =
    ( sequrityTitle.map( sequrities ).map( _.longValue ).map( _.formatted("%5d") ) )
      // ^ change longValue if you need change data types of values
      .mkString( name.formatted("%5s\t"), "\t", "")

  // выполняет заказ у клиента
  def process( order: Order ) = {

    assert( name==order.client )  // вызывая эту функцию необходимо передать заказ именно этого клиента
    assert( sequrities.contains(order.sequrity) ) // ценная бумага заказа должна быть известна

    // Определение направления перемещения ценных бумаг
    val sign = order.op match {
      case "b" => +1  // покупатель приобретает
      case "s" => -1  // продавец отдаёт
      case _ => assert(false); 0
    }

    // собственно сделка
    //synchronized { sequrities.put( order.sequrity, sequrities.getOrElse(order.sequrity,0) + sign*order.amount ) }
    sequrities( order.sequrity ).addAndGet( sign*order.amount )       // передача бумаг
    sequrities( money ).addAndGet( -sign*order.price*order.amount )   // передача денег в обратном направлении
  }

}

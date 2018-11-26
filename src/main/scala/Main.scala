import scala.annotation.tailrec
import scala.collection.GenTraversableOnce
import scala.io.Source

import java.io.{FileOutputStream, OutputStreamWriter, PrintWriter}


object Main extends App {

  val fclients  = "./clients.txt"  // исходный файл клиентов с балансами
  val forders   = "./orders.txt"   // исходный файл заявок
  val foutput   = "./output.txt"   // результат - файл состояния в конце
  val enc       = "cp1251"         // кодировка файлов (ввод-вывод)

  // -----------------------------
  // контроль состояний всех клиентов (для отладки)
  def prnitClients() = {
    println( "\t"+Client.header )
    clients.toList.sortBy(_._1).map(_._2).map(s=>s"\t$s").foreach( println )
    // суммы
    println( "\t\t\t"+
      Settings.sequrityTitle.map(
        clients.map(_._2.sequrities.mapValues(_.get).toMap).reduce {
          (map1:Map[String,Long],map2:Map[String,Long]) => map1++map2.map { case (k,v) => k -> ( v + map1.getOrElse(k,0L) ) }
        }
      ).map( _.formatted("%5d") ).mkString("\t")
    )
    println
  }
  // -----------------------------

  println( "HELLO\n" ) // признак начала работы
  val time0 = System.currentTimeMillis

  // Загружаем клиентов
  val clients = Source.fromFile(fclients, enc).getLines().map( new Client(_) )
    .map( c => c.name->c ).toMap

  prnitClients() // Начальное состояние транзакций клиентов

  // Загружаем заявки
  val orders = Source.fromFile(forders, enc).getLines().map( new Order(_) )
    .filter( order => clients.keySet.contains(order.client) ) // только от известных клиентов
    .filter( order => Set("b","s").contains(order.op) )       // только с понятными заявками
    .toSet

  println( orders.size + " orders are loaded and accepted." )
  println

  // [in] заявки, которые надо выполнить
  // return: невыполненные заявки и кол-во выполненных заявок
  @tailrec
  def process( orders: Set[Order], cnt: Int=0 ): (Set[Order], Int) = {

    // Ленивое декартово произведение
    // NB при соединении cross в цепочки (размерноесть больше 2-х) делает вложенные Tuple2
    implicit class lazyCrossable[X](xs: GenTraversableOnce[X]) {
      def cross[Y](ys: GenTraversableOnce[Y]) =
        for { x <- xs.toIterator; y <- ys.toIterator } yield (x, y)
    }

    orders cross orders find { case (a,b) => a matches b } match {
      // matches сам проверяет что операция зявок должна быть различна
      // и что клиенты заявок не совпадают

      case None => (orders, cnt) // более нет выполнимых заявок
      case Some( (a,b) ) =>      // обнаружена сопоставимая пара заявок

        // транзакция:
        synchronized {
          clients.get(a.client).get.process(a)
          clients.get(b.client).get.process(b)
          // сейчас сопоставляются заявки с полными соответствиями цены
          // если снять это условие (принимать пары заявок с ценой продажи < цены покупки
          // в process-ы вероятно понадобится передавать ещё и цену сделки
          // (сейчас она совпадает с ценой заявки)
        }

        // контроль
        println( s"${cnt + 1}\t : Orders match: { $a } + { $b }" )
        prnitClients()

        process( orders - a - b, cnt+1 ) // continue
    }
  }

  val ( rest, processed ) = process( orders ) // process orders

  // short report
  println( s"$processed transactions (${processed*2} orders) are processed." )
  println( s"Rest: ${rest.size} orders." )

  // save result
  val osw = new OutputStreamWriter(new FileOutputStream(foutput), enc)
  try {
    val pw = new PrintWriter(osw)
    try {
      clients.toList.sortBy(_._1).map(_._2).map( _.toString.trim ).foreach( pw.println )
    }
    finally {
      pw.close()
    }
  }
  finally {
    osw.close()
  }

  val time1 = System.currentTimeMillis
  println( s"BYE. Elapsed ${time1-time0} ms" )   // признак конца работы
}

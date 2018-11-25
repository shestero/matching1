
// класс Заявки

case class Order(client: String, op:String, sequrity: String, price: Long, amount: Int) {

  // ещё вспомогательный конструктор - для чтения из строки TSV
  protected def this( arr: Array[String] ) = {
    this(
      arr.toList.head, // client
      arr.toList.tail.head, // operation (s or b)
      arr.toList.tail.tail.head, // sequrity
      arr.toList.tail.tail.tail.head.toLong, // price of one peace
      // ^ Long промежуточный тип; (исправить в случае изменения на Double итп)
      arr.toList.tail.tail.tail.tail.head.toInt ) // amount of sequrity
    assert( arr.size == 5 )
  }
  def this( line: String ) = this( line.stripLineEnd.split("\t") ) // .getLines.map(_.stripLineEnd.split("\t", -1))

  // порверяет, что пара заявкок удовлетворяет друг друга
  def matches( other: Order ) = {
      client != other.client &&       // нельзя продавать самому себе
      op != other.op &&               // флаг продажи/покупки заявок должен быть разным
      sequrity == other.sequrity &&   // речь должна быть про одну бумагу
      // priceBue<=priceSale &&
      price==other.price &&
      amount==other.amount            // это версия с полным соответствием цена-кол-во
  }

  // вывод в формате строки читаемого TSV
  override def toString: String = ( List(client) :+ op :+ sequrity :+ price :+ amount ).mkString("\t")

}

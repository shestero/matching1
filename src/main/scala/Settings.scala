// Общие константы и типы

object Settings {
  val money = "$" // главная ценная бумага :-)
  val sequrityTitle = List( money, "A", "B", "C", "D" )    // виды ценностей, которые могут быть у клиентов
  type ValueType = java.util.concurrent.atomic.AtomicLong  // Тип единиц измерения ценностей
}

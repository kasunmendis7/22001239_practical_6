case class Product(id: Int, name: String, quantity: Int, price: Double)

object Question1 {

  val inventory1: Map[Int, Product] = Map(
    101 -> Product(101, "ProductA", 10, 50.0),
    102 -> Product(102, "ProductB", 5, 30.0)
  )

  val inventory2: Map[Int, Product] = Map(
    102 -> Product(102, "ProductB", 3, 35.0),
    103 -> Product(103, "ProductC", 7, 20.0)
  )

  def getProductNames(inventory: Map[Int, Product]): List[String] = {
    inventory.values.map(_.name).toList
  }

  def calculateTotalValue(inventory: Map[Int, Product]): Double = {
    inventory.values.map(product => product.quantity * product.price).sum
  }
  
  def main(args: Array[String]): Unit = {}

}

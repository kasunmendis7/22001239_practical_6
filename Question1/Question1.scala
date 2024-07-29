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

  def displayProducts(inventory: Map[Int, Product]): List[String] = {
    inventory.values.map(_.name).toList
  }

  def calculateTotalValue(inventory: Map[Int, Product]): Double = {
    inventory.values.map(product => product.quantity * product.price).sum
  }

  def isInventoryEmpty(inventory: Map[Int, Product]): Boolean = {
    inventory.isEmpty
  }

  def mergeInventories(inventory1: Map[Int, Product], inventory2: Map[Int, Product]): List[Product] = {
  val mergedInventory = inventory2.foldLeft(inventory1) { case (acc, (id, product)) =>
    acc.get(id) match {
      case Some(existingProduct) =>
        acc.updated(id, Product(id, product.name, existingProduct.quantity + product.quantity, math.max(existingProduct.price, product.price)))
      case None =>
        acc + (id -> product)
    }
  }
  mergedInventory.values.toList
}

  def checkProductExists(inventory: Map[Int, Product], productId: Int): Unit = {
    inventory.get(productId) match {
      case Some(product) => println(s"Product found: $product")
      case None          => println(s"Product with ID $productId not found.")
    }
  }

  def main(args: Array[String]): Unit = {}
  while (true) {

    println("1.View all the products");
    println("2.Calculate Total value of products")
    println("3.Check is inventory is empty")
    println("4.Merge inventories and update products")
    println("5.Check if a product exists")
    println("6.Exit system")
    print("Enter your choice: ")
    var choice = scala.io.StdIn.readInt()
    choice match {

      case 1 =>
        print("Inventory 1 or 2. Enter the number: ")
        val num = scala.io.StdIn.readInt();
        if (num == 1)
          println("Product Names: " + displayProducts(inventory1))
        else if (num == 2)
          println("Product Names: " + displayProducts(inventory2))
        else
          println("Inventory does not exist")
      case 2 =>
        print("Inventory 1 or 2. Enter the number: ")
        val num = scala.io.StdIn.readInt();
        if (num == 1)
          println("Total Value: " + calculateTotalValue(inventory1))
        else if (num == 2)
          println("Total Value: " + calculateTotalValue(inventory2))
        else
          println("Inventory does not exist")
      case 3 =>
        print("Inventory 1 or 2. Enter the number: ")
        val num = scala.io.StdIn.readInt();
        if (num == 1)
          println("Is Inventory Empty: " + isInventoryEmpty(inventory1))
        else if (num == 2)
          println("Is Inventory Empty: " + isInventoryEmpty(inventory2))
        else
          println("Inventory does not exist")
      case 4 =>
        val mergedInventory = mergeInventories(inventory1, inventory2)
        println("Merged Inventory: " + mergedInventory)
      case 5 =>
        print("Inventory 1 or 2?\nEnter the number: ")
        val num = scala.io.StdIn.readInt();
        if (num == 1) {
          print("Enter product ID: ")
          val id = scala.io.StdIn.readInt();
          println("Is Inventory Empty: " + checkProductExists(inventory1, id))
        } else if (num == 2) {
          print("Enter product ID: ")
          val id = scala.io.StdIn.readInt();
          println("Is Inventory Empty: " + checkProductExists(inventory2, id))
        } else
          println("Inventory does not exist")
      case 6 =>
        println("Exiting system")
        System.exit(0)
      case _ =>
        println("Invalid input. Please try again")

    }

  }

}

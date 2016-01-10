case class Coffee(price: Double = 2.50)
class CreditCard {
  val name: String = "Chase"
  var balance: Double = 100
  def charge(amount: Double): Unit = 
    balance -= amount
  
}
case class Charge(cc: CreditCard, price: Double) {
  def combine(other: Charge): Charge = {
    if (cc==other.cc)
      Charge(cc, price + other.price)
    else
      throw new Exception("Cannot combine differnt cc charges")
  }
  
}

// Side effect function
class Cafe1 {
  def buyCoffee(cc: CreditCard): Coffee = {
    val coffee = Coffee()
    // charge the cc
    cc.charge(coffee.price)
    coffee
  }
}

// no side effect function
class Cafe2 {
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    // buy the coffee, then also return the Charge
    val coffee = Coffee()
    val charge = Charge(cc, coffee.price)
    (coffee, charge)
  }
  
  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    // create n coffee requests
    List.fill(n)(buyCoffee(cc)).unzip match { case (coffees: List[Coffee], charges: List[Charge]) =>
        (coffees, charges.reduce((a: Charge, b: Charge)=> a.combine(b)))
    }
  }
}

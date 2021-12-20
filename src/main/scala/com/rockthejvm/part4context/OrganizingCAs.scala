package com.rockthejvm.part4context

object OrganizingCAs {

  val aList = List(2,3,1,4)
  val anOrderedList = aList.sorted

  // how compiler fetches givens/extension methods (implicits in Scala 2)

  // 1 - local scope
  given reverseOrdering: Ordering[Int] with
    override def compare(x: Int, y: Int) = y - x


  // 2 - explicitly imported scope
  case class Person(name: String, age: Int)
  val persons = List(
    Person("Steve", 30),
    Person("Amy", 22),
    Person("John", 67)
  )

  object PersonGivens {
    given ageOrdering: Ordering[Person] with
      override def compare(x: Person, y: Person) = y.age - x.age

    extension (p: Person)
      def greet(): String = s"Heya, I'm ${p.name}, I'm so glad to meet you!"
  }

  // a - import explicitly
  // import PersonGivens.ageOrdering

  // b - import a given for a particular type
  import PersonGivens.{given Ordering[Person]}

  // c - import all givens
  // import PersonGivens.given

  // warning: import PersonGivens.* does NOT also import given instances!


  // 3 - companions of all types involved in method signature
  /*
    - Ordering
    - List
    - Person
   */
  // def sorted[B >: A](using ord: Ordering[B]): List[B]

  object Person {
    given byNameOrdering: Ordering[Person] with
      override def compare(x: Person, y: Person) =
        x.name.compareTo(y.name)

    extension (p: Person)
      def greet(): String = s"Hello, I'm ${p.name}."
  }

  val sortedPersons = persons.sorted

  /*
    Good practice tips:
    1) When you have a "default" given (only ONE that makes sense) add it in the companion object of the type
    2) When you have MANY possible givens, but ONE that is dominant (used most), add that in the companion and the rest in other objects
    3) When you have MANY possible givens and NO ONE is dominant, add them in separate objects and import them explicitly
   */

  // Same principles apply to extension methods as well

  // implicit Scala 2 keyword is the same as using Scala 3 keyword



  /**
   * Exercises. Create given instances for Ordering[Purchase]
   * - ordering by total price, descending = 50% of code base
   * - ordering by unit count, descending = 25% of code base
   * - ordering by unit price, ascending = 25% of code base
   */
  case class Purchase(nUnits: Int, unitPrice: Double)

  object Purchase {
    given totalPriceOrdering: Ordering[Purchase] with
      override def compare(x: Purchase, y: Purchase): Int = {
        val xTotalPrice = x.nUnits * x.unitPrice
        val yTotalPrice = y.nUnits * y.unitPrice

        if xTotalPrice == yTotalPrice then 0
        else if xTotalPrice < yTotalPrice then -1
        else 1
      }
  }

  object UnitCountOrdering {
    given unitCountOrdering: Ordering[Purchase] = Ordering.fromLessThan(_.nUnits > _.nUnits)
  }

  object UnitPriceOrdering {
    given unitPriceOrdering: Ordering[Purchase] = Ordering.fromLessThan(_.unitPrice < _.unitPrice)
  }


  def main(args: Array[String]): Unit = {
    println(anOrderedList)
    println(sortedPersons)
    import PersonGivens.* // includes extension methods
    println(Person("Daniel", 99).greet())
  }
}
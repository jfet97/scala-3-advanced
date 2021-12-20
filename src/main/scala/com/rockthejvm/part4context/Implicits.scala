package com.rockthejvm.part4context

object Implicits {

  // the old way (still supported in Scala 3)

  // the ability to pass arguments automatically (implicitly) by the compiler
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  def combineAll[A](list: List[A])(implicit semigroup: Semigroup[A]): A =
    list.reduce(semigroup.combine)

  implicit val intSemigroup: Semigroup[Int] = new Semigroup[Int] {
    override def combine(x: Int, y: Int) = x + y
  }

  val sumOf10 = combineAll((1 to 10).toList)

  // implicit arg -> using clause
  // implicit val -> given declaration


  // implicit class (Scala 2) -> extension methods (Scala 3)
  implicit class MyRichInteger(number: Int) {
    // extension methods here
    def isEven = number % 2 == 0
  }

  val questionOfMyLife = 23.isEven // new MyRichInteger(23).isEven


  // implicit conversions
  case class Person(name: String) {
    def greet(): String = s"Hi, my name is $name."
  }

  // implicit conversion - SUPER DANGEROUS, very hard to debug
  implicit def string2Person(x: String): Person = Person(x)
  val danielSaysHi = "Daniel".greet() // string2Person("Daniel").greet()


  // implicit def main goal was: synthesize new implicit values
  implicit def semigroupOfOption[A](implicit semigroup: Semigroup[A]): Semigroup[Option[A]] = new Semigroup[Option[A]] {
    override def combine(x: Option[A], y: Option[A]) = for {
      valueX <- x
      valueY <- y
    } yield semigroup.combine(valueX, valueY)
  }

  /*
    Equivalent:
      given semigroupOfOption[A](using semigroup: Semigroup[A]): Semigroup[Option[A]] with ...
   */


  // organizing implicits == organizing contextual abstractions (same principles)
  // import yourPackage.* // also imports implicits :(

  /*
    Why implicits will be phased out:
    - the implicit keyword has many different meanings
    - conversions are easy to abuse
    - implicits are very hard to track down while debugging (givens also not trivial, but they are explicitly imported)
   */

  /*
    The right way of doing contextual abstractions in Scala 3:
    - given/using clauses
    - extension methods
    - explicitly declared implicit conversions
   */

  def main(args: Array[String]): Unit = {
    println(sumOf10)
  }
}
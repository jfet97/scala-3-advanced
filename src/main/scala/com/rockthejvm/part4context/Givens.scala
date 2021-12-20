package com.rockthejvm.part4context

object Givens {

  // list sorting
  val aList = List(4,1,2,3)
  val anOrderedList = aList.sorted // uses the `descendingOrdering` given

  // a signal to the compiler to automatically inject `Ordering.fromLessThan(_ > _)`
  // into all methods that require the presence of an Ordering[Int] in scope
  given descendingOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _) // glorified comparator
  val anInverseOrderedList = aList.sorted(descendingOrdering)


  // custom sorting
  case class Person(name: String, age: Int)
  val people = List(Person("Alice", 29), Person("Sarah", 37), Person("Jim", 34))

  given personOrdering: Ordering[Person] = new Ordering[Person] {
    override def compare(x: Person, y: Person): Int =
      x.name.compareTo(y.name)
  }

  val orderedPeople = people.sorted // not needed explicit passing of the Ordering[Person] because of the given


  object PersonAltSyntax {
    // same as above, but inside an object because we cannot have two same given in the same scope
    given personOrdering: Ordering[Person] with {
      override def compare(x: Person, y: Person): Int =
        x.name.compareTo(y.name)
    }
  }


  // using clauses
  trait Combinator[A] {
    def combine(x: A, y: A): A
  }

  // whenever we use the following method, the compiler will require a
  // specific given combinator in scope
  def combineAll[A](list: List[A])(using combinator: Combinator[A]): A =
    list.reduce(combinator.combine)


  val firstSum = combineAll(List(1,2,4,3)) // error without a proper given Combinator[Int]
  given intCombinator: Combinator[Int] with {
    override def combine(x: Int, y: Int): Int = x + y
  }

  // val combineAllPeople = combineAll(people) // error without a proper given Combinator[People]


  // context bound
  def combineInGroupsOf3[A](list: List[A])(using Combinator[A]): List[A] =
    list.grouped(3).map(combineAll).toList // combinator passed automatically to combineAll, actually no name needed

  def combineInGroupsOf3_v2[A : Combinator](list: List[A]): List[A] = // A : Combinator => there is a given Combinator[A] in scope
    list.grouped(3).map(combineAll).toList // combinator passed automatically to combineAll, actually no name needed


  // synthesize new given instance based on existing ones
  given listOrdering(using intOrdering: Ordering[Int]): Ordering[List[Int]] with {
    override def compare(x: List[Int], y: List[Int]): Int =
      x.sum - y.sum
  }

  val listOfLists = List(List(1,2), List(1,1), List(3,4,5))
  val nestedListsOrdered = listOfLists.sorted

  // ..with generics: for each A we have a given Ordering[List[A]] everytime there is a Ordering[A] and a Combinator[A] in scope
  given listOrderingBasedOnCombinator[A](using ord: Ordering[A])(using comb: Combinator[A]): Ordering[List[A]] with {
    override def compare(x: List[A], y: List[A]): Int =
      ord.compare(combineAll(x), combineAll(y)) // comb used by combineAll
  }

  // pass a regular value instead of a given
  val myCombinator = new Combinator[Int] {
    override def combine(x: Int, y: Int): Int = x * y
  }
  val listProduct = combineAll(List(1,2,3,4))(using myCombinator)




  /**
   * Exercises:
   * 1 - create a given for ordering Option[A] if you can order A
   * 2 - create a summoning method that fetches the given value of your particular type
   */

  // 1
  given optionOrdering[A](using ord: Ordering[A]): Ordering[Option[A]] with {
    override def compare(ox: Option[A], oy: Option[A]): Int = (ox, oy) match {
      case (None, None) => 0
      case (None, _) => -1
      case (_, None) => 1
      case (Some(x), Some(y)) => ord.compare(x, y)
    }
  }

  // 2
  object newScope {
    given optionOrdering_v2[A : Ordering]: Ordering[Option[A]] with {
      override def compare(ox: Option[A], oy: Option[A]): Int = (ox, oy) match {
        case (None, None) => 0
        case (None, _) => -1
        case (_, None) => 1
        case (Some(x), Some(y)) => fetchGivenValue[Ordering[A]].compare(x, y) // we know that there is a Ordering[A] but we cannot directly fetch it
      }
    }
  }

  // if we can actually call fetchGivenValue, the compiler has access to whatever
  // given instance might have been declared in some other places in our code
  // so we can actually return that instance to be used later
  //
  // we can use explicitly an implicit given
  // in the standard library the name of the function is `summon`
  def fetchGivenValue[A](using theValue: A): A = theValue

  def main(args: Array[String]): Unit = {
    println(anOrderedList) // [1,2,3,4]
    println(anInverseOrderedList) // [4,3,2,1]

    println(people)
    println(orderedPeople)

    println(nestedListsOrdered)

    println(listProduct)

    println(List(Option(1), Option.empty[Int], Option(3), Option(-1000)).sorted)

  }
}

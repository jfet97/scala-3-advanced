package com.rockthejvm.part1as

object AdvancedPatternMatching {

  /*
    pattern matching works with:
    - constants
    - objects
    - wildcards
    - variables
    - infix patterns
    - lists
    - case classes
   */


  // custom pattern matching
  class Person(val name: String, val age: Int) // not a case class

  // name of the object === name of the pattern
  object Person {
    // unapply takes only one parameter

    def unapply(person: Person): Option[(String, Int)] = // person match { case Person(string, int) => } is now available
      if (person.age < 21) None
      else Some((person.name, person.age))

    // we can subject an int to a Person pattern match which will return a String
    def unapply(age: Int): Option[String] = // int match { case Person(string) => ... } is now available
      if (age < 21) Some("minor")
      else Some("legally allowed to drink")
  }

  val daniel = new Person("Daniel", 102)

  val danielPM: String = daniel match { // same as Person.unapply(daniel) => Option((n, a))
    case Person(n, a) => s"Hi there, I'm $n" // name of the pattern is Person because of the name of the (companion) object
    // n, a are the (String, Int) returned by the Person.unapply(Person) method
  }
  val danielsLegalStatus: String = daniel.age match {
    case Person(status) => s"Daniel's legal drinking status is $status"
    // status is the String returned by the Person.unapply(Int) method
  }


  // boolean patterns
  object even {
    def unapply(arg: Int): Boolean = arg % 2 == 0
  }

  object singleDigit {
    def unapply(arg: Int): Boolean = arg > -10 && arg < 10
  }

  val n: Int = 43
  val mathProperty: String = n match {
    case even() => "an even number"
    case singleDigit() => "a one digit number"
    case _ => "no special property"
  }
  // nothing to deconstruct inside the parenthesis because unapply methods don't return an Option[Boolean]
  // the argument of the unapply methods is an Int, so n will be correctly handled by the patterns


  // infix patterns
  infix case class Or[A, B](a: A, b: B)
  val anEither: Int Or String = Or(2, "two")
  val humanDescriptionEither: String = anEither match {
    case number Or string => s"$number is written as $string" // same as the Or(number, string) pattern
  }
  // Oss. a case class automatically has a companion object which implements the unapply method

  val aList = List(1,2,3)
  val listPM: String = aList match {
    case 1 :: rest => "a list starting with 1"
    case _ => "some uninteresting list"
  }
  // there is a :: object which implements the unapply method


  // decomposing sequences
  val vararg: String = aList match {
    case List(1, _*) => "list starting with 1"
    case _ => "some other list"
  }

  abstract class MyList[A] {
    def head: A = throw new NoSuchElementException
    def tail: MyList[A] = throw new NoSuchElementException
  }
  case class Empty[A]() extends MyList[A]
  case class Cons[A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList {
    // enables varargs pattern matching on any kind of list (its generic)
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty()) Some(Seq.empty)
      else unapplySeq(list.tail).map(restOfSequence => list.head +: restOfSequence) // prepend after the recursive call
      // => _* will be the rest of the list
  }

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty())))
  val varargCustom: String = myList match {
    case MyList(1, _*) => "list starting with 1"
    case _ => "some other list"
  }


  // custom return type for unapply (Option is
  abstract class Wrapper[T] {
    // these are the needed methods
    def isEmpty: Boolean
    def get: T
  }

  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      override def isEmpty = false
      override def get: String = person.name
    }
  }

  val weirdPersonPM: String = daniel match {
    case PersonWrapper(name) => s"Hey my name is $name"
  }


  def main(args: Array[String]): Unit = {
    println(danielPM)
    println(danielsLegalStatus)
    println(mathProperty)
  }
}
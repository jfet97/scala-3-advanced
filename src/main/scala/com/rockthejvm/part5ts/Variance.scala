package com.rockthejvm.part5ts

object Variance {

  class Animal
  class Dog(name: String) extends Animal
  class Cat extends Animal

  // Variance question for List: if Dog extends Animal, then should a List[Dog] "extend" List[Animal]?

  // for List, YES - List is COVARIANT
  val lassie = new Dog("Lassie")
  val hachi = new Dog("Hachi")
  val laika = new Dog("Laika")

  val anAnimal: Animal = lassie // ok because Dog <: Animal (`<:` means subtype, `:>` means supertype)
  val myDogs: List[Animal] = List(lassie, hachi, laika) // ok - List is COVARIANT: a list of dogs is a list of animals

  // define covariant types
  class MyList[+A] // MyList is COVARIANT in A
  val aListOfAnimals: MyList[Animal] = new MyList[Dog]

  // if the answer is NO, then the type is INVARIANT
  trait Semigroup[A] { // no marker = INVARIANT
    def combine(x: A, y: A): A
  }

  // all java generics but arrays are all INVARIANT

  // if the answer is Hell NO - CONTRAVARIANCE
  trait Vet[-A] { // contravariant in A
    def heal(animal: A): Boolean
  }

  val myVet: Vet[Dog] = new Vet[Animal] {
    override def heal(animal: Animal) = {
      println("Hey there, you're all good...")
      return true
    }
  }
  // if the vet can treat any animal, she/he can treat my dog too
  val healLaika = myVet.heal(laika) // ok



  /*
    Rule of thumb:
    - if your type PRODUCES or RETRIEVES a value (e.g. a list), then it should be COVARIANT: the type parameter is only used in a return position
    - if your type ACTS ON or CONSUMES a value (e.g. a vet), then it should be CONTRAVARIANT: he type parameter is only used in an argument position
    - otherwise, INVARIANT: it uses its type parameter both as an argument and as a return type
   */



  /**
   * Exercises
   */
  // 1 - which types should be invariant, covariant, contravariant
  class RandomGenerator[+A] // produces values: Covariant
  class MyOption[+A] // similar to Option[A]
  class JSONSerializer[-A] // consumes values and turns them into strings: Contravariant
  trait MyFunction[-A, +B]  // similar to Function1[A, B]

  // 2 - add variance modifiers to this "library"
  abstract class LList[+A] {
    def head: A
    def tail: LList[A]
  }

  case object EmptyList extends LList[Nothing] {
    override def head = throw new NoSuchElementException
    override def tail = throw new NoSuchElementException
  }

  case class Cons[+A](override val head: A, override val tail: LList[A]) extends LList[A]

  val aList: LList[Int] = EmptyList // fine
  val anotherList: LList[String] = EmptyList // also fine
  // Nothing <: A, then LList[Nothing] <: LList[A]


  def main(args: Array[String]): Unit = {

  }
}

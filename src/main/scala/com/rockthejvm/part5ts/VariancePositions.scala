package com.rockthejvm.part5ts

object VariancePositions {

  class Animal
  class Dog extends Animal
  class Cat extends Animal
  class Crocodile extends Animal



  /**
   * 1 - type bounds
   */

  class Cage[A <: Animal] // A must be a subtype of Animal
  val aRealCage = new Cage[Dog] // ok, Dog <: Animal
  // val aCage = new Cage[String] // not ok, String is not a subtype of animal

  class WeirdContainer[A >: Animal] // A must be a supertype of Animal



  /**
   * 2 - variance positions
   */

  // types of val and var fields are in COVARIANT position
  // class Vet[-T](var favoriteAnimal: T)
  // -T means it consumes values of type T, but if something of type T is a field we can retrieve it
  // as if Vet would produce values of type T

  /*
    If the class would compile we would able to do:

    val garfield = new Cat
    val theVet: Vet[Animal] = new Vet[Animal](garfield)
    val aDogVet: Vet[Dog] = theVet // possible, theVet is Vet[Animal] + contravariance
    val aDog: Dog = aDogVet.favoriteAnimal // must be a Dog but it is a Cat - type conflict!
   */


  // types of var fields are in CONTRAVARIANT position as well
  // class MutableOption[+T](var contents: T)

  /*
    If the class would compile we would able to do:

    val maybeAnimal: MutableOption[Animal] = new MutableOption[Dog](new Dog)
    maybeAnimal.contents = new Cat // type conflict!
  */


  // => we can safely assume that val fields are covariant because of immutability (read-only)
  // => for var fields we need invariance because we can both read and write those fields


  // types of method arguments are in CONTRAVARIANT position because they are consumed
  //  class MyList[+T] {
  //    def add(element: T): MyList[T] = ???
  //  }

  // types of method return values are in COVARIANT position because they are produced
  //  class MyList[-T] {
  //    def rescueAnimal(): T = ???
  //  }



  /**
   * 3 - solving variance positions problems
   */

  // 1
  abstract class LList[+A] {
    def head: A
    def tail: LList[A]
    def add[B >: A](element: B): LList[B] // widen the type
  }

  case class Cons[+A](head: A, tail: LList[A]) extends LList[A] {
    override def add[B >: A](element: B): LList[B] = Cons(element, this)
  }

  case object Empty extends LList[Nothing] {
    override def head: Nothing = ???
    override def tail: LList[Nothing] = ???
    override def add[B >: Nothing](element: B): LList[B] = Cons(element, this)
  }

  val cats: LList[Cat] = Cons(new Cat, Cons(new Cat, Empty))
  val animals = cats.add(new Dog) // the compiler automatically infers the lowest common ancestor, that is LList[Animal] because of the covariance


  // 2
  class Vehicle
  class Car extends Vehicle
  class Supercar extends Car
  class RepairShop[-A <: Vehicle] { // contravariant because the main purpose is to consume vehicles to be repaired
    def repair[B <: A](vehicle: B): B = vehicle // narrowing the type
  }

  val myRepairShop: RepairShop[Car] = new RepairShop[Vehicle] // a RepairShop[Vehicle] can repair any car
  val myBeatupVW = new Car
  val freshCar = myRepairShop.repair(myBeatupVW) // works, returns a car
  val damagedFerrari = new Supercar
  val freshFerrari = myRepairShop.repair(damagedFerrari) // works, returns a Supercar
  // the compiler automatically infers the correct subtype

  def main(args: Array[String]): Unit = {

  }
}

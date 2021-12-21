package com.rockthejvm.part5ts

object TypeMembers {

  class Animal
  class Dog extends Animal
  class Cat extends Animal

  class AnimalCollection {
    // we can define types

    type AnimalType // abstract type member
    type BoundedAnimal <: Animal // abstract type member with a type bound
    type SuperBoundedAnimal >: Dog <: Animal
    type AnimalAlias = Cat // type alias
    type NestedOption = List[Option[Option[Int]]] // often used to alias complex/nested types
  }

  class MireConcreteAnimalCollection extends AnimalCollection {
    // we can override types

    override type AnimalType = Dog
  }

  // using type members
  val ac = new AnimalCollection
  val anAnimal: ac.AnimalType = ??? // every instance of AnimalCollection has its own type definitions

  // val cat: ac.BoundedAnimal = new Cat // BoundedAnimal might be Dog, now it is abstract but it will be only one concrete type
  val aDog: ac.SuperBoundedAnimal = new Dog // ok, Dog <: SuperBoundedAnimal
  val aCat: ac.AnimalAlias = new Cat // ok, Cat == AnimalAlias

  // establish relationships between types
  // alternative to generics
  class LList[T] {
    def add(element: T): LList[T] = ???
  }

  class MyList {
    type T // every instance of MyList has to have its own type definition for T
    def add(element: T): MyList = ???
  }

  // .type (can be used on every value)
  type CatType = aCat.type
  val newCat: CatType = aCat

  def main(args: Array[String]): Unit = {

  }
}
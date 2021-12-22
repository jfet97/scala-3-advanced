package com.rockthejvm.part5ts

import reflect.Selectable.reflectiveSelectable // <- needed for the compiler

object StructuralTypes {

  type SoundMaker = { // structural type: defined by its content, not by its name
    def makeSound(): Unit
  }

  class Dog {
    def makeSound(): Unit = println("bark!")
  }

  class Car {
    def makeSound(): Unit = println("vroom!")
  }

  val dog: SoundMaker = new Dog // ok
  val car: SoundMaker = new Car
  // compile-time duck typing


  // type refinements
  abstract class Animal {
    def eat(): String
  }

  type WalkingAnimal = Animal { // refined type (we are extending Animal with a structural type)
    def walk(): Int
  }


  // why: creating type-safe APIs for existing types following the same structure, but no connection to each other
  type JavaCloseable = java.io.Closeable
  class CustomCloseable {
    def close(): Unit = println("ok ok I'm closing")
    def closeSilently(): Unit = println("not making a sound, I promise")
  }

  //  def closeResource(closeable: JavaCloseable | CustomCloseable): Unit =
  //    closeable.close() // not ok because the compiler cannot figure out the similarities between a JavaCloseable and a CustomCloseable

  // solution: structural type
  type UnifiedCloseable = {
    def close(): Unit
  }

  // unified API: any type which conforms to the structure of UnifiedCloseable is a legitimate argument
  def closeResource(closeable: UnifiedCloseable): Unit = closeable.close()
  val jCloseable = new JavaCloseable {
    override def close(): Unit = println("closing Java resource")
  }
  val cCloseable = new CustomCloseable

  // same unified API, with the structural type inline
  def closeResource_v2(closeable: { def close(): Unit }): Unit = closeable.close()
  //                              |                   |
  //                              |  this is a type:  |
  //
  //                              |  structural type |

  def main(args: Array[String]): Unit = {
    dog.makeSound() // done through reflection (slow)
    car.makeSound() // done through reflection (slow)

    closeResource(jCloseable)
    closeResource(cCloseable)
  }
}
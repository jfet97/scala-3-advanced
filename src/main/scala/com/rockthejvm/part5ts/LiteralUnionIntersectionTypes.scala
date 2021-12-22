package com.rockthejvm.part5ts

import java.io.File

object LiteralUnionIntersectionTypes {

  // 1 - literal types
  val aNumber = 3
  val three: 3 = 3

  def passNumber(n: Int) = println(n)
  passNumber(45) // ok
  passNumber(three) // ok, 3 <: Int

  def passStrict(n: 3) = println(n)
  passStrict(three) // ok
  // passStrict(45) // not ok, Int <: 3

  // available for double, boolean, strings
  val pi: 3.14 = 3.14
  val truth: true = true
  val favLang: "Scala" = "Scala"

  // literal types can be used as type arguments (just like any other types)
  def doSomethingWithYourLife(meaning: Option[42]) = meaning.foreach(println)



  // 2 - union types
  val truthor42: Boolean | Int = 43

  def ambivalentMethod(arg: String | Int): String = arg match {
    case s: String => s
    case _: Int => "a number"
  }

  val aNumberDescription = ambivalentMethod(56) // ok
  val aStringDescription = ambivalentMethod("Scala") // ok

  // type inference chooses a lowest common ancestor of the two types instead of the String | Int
  val stringOrInt = if (43 > 0) "a string" else 45
  val stringOrInt_v2: String | Int = if (43 > 0) "a string" else 45 // ok

  // union types + nulls
  type Maybe[T] = T | Null // is the type of the null value
  def handleMaybe(someValue: Maybe[String]): Int =
    if (someValue != null) someValue.length // flow typing (refinement)
    else 0

  type ErrorOr[T] = T | "error"
  //  def handleResource(arg: ErrorOr[Int]): Unit =
  //    if (arg != "error") println(arg + 1) // flow typing doesn't work here :(
  //    else println("Error!")



  // 3 - intersection types
  class Animal
  trait Carnivore
  class Crocodile extends Animal with Carnivore
  // a type that is both an Animal and a Carnivore
  val carnivoreAnimal: Animal & Carnivore = new Crocodile

  trait Gadget {
    def use(): Unit
  }

  trait Camera extends Gadget {
    def takePicture() = println("smile!")
    override def use() = println("snap")
  }

  trait Phone extends Gadget {
    def makePhoneCall() = println("calling...")
    override def use() = println("ring")
  }

  def useSmartDevice(sp: Camera & Phone): Unit = {
    sp.takePicture()
    sp.makePhoneCall()
    sp.use() // which use() is being called? can't tell, it depends on the actual type of `sp`
  }

  class SmartPhone extends Phone with Camera // diamond problem
  class CameraWithPhone extends Camera with Phone


  // intersection types + covariance
  trait HostConfig
  trait HostController {
    def get: Option[HostConfig]
  }

  trait PortConfig
  trait PortController {
    def get: Option[PortConfig]
  }

  def getConfigs(controller: HostController & PortController): Option[HostConfig & PortConfig] = controller.get
  // it compiles, when the compiler sees an intersection as an argument type and there are conflicts
  // like these it tries to do the same for the return type
  // Option[HostConfig] & Option[PortConfig] <: Option[HostConfig & PortConfig] because of the covariance of the Option type
  // Option[HostConfig & PortConfig] is a supertype both of Option[HostConfig] and Option[PortConfig], so if something
  // is both we have that Option[HostConfig & PortConfig] remains a supertype of that something

  def main(args: Array[String]): Unit = {
    useSmartDevice(new SmartPhone) // "snap"
    useSmartDevice(new CameraWithPhone) // "ring"
  }
}
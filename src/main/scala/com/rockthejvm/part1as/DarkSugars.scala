package com.rockthejvm.part1as

import scala.util.Try

object DarkSugars {

  // 1 - sugar for methods with one argument
  def singleArgMethod(arg: Int): Int = arg + 1

  val aMethodCall: Int = singleArgMethod({
    // long code

    42 // <- singleArgMethod will receive this as argument
  })

  val aMethodCall_v2: Int = singleArgMethod {
    // long code

    42 // <- singleArgMethod will receive this as argument
  }

  // example: Try, Future
  val aTryInstance: Try[Nothing] = Try {
    throw new RuntimeException
  }

  // with hofs
  val anIncrementedList: List[Int] = List(1,2,3).map { x =>
    // code block
    x + 1
  }


  // 2 - single abstract method pattern (since Scala 2.12)
  trait Action {
    // can also have other implemented fields/methods here
    // but only one abstract method
    def act(x: Int): Int
  }

  val anAction: Action = new Action {
    override def act(x: Int): Int = x + 1
  }

  val anotherAction: Action = (x: Int) => x + 1 // same as new Action { def act(x: Int) = x + 1 }

  // example: Runnable
  val aThread = new Thread(new Runnable { // <- the pattern will be applied on Runnable
    override def run(): Unit = println("Hi, Scala, from another thread")
  })

  val aSweeterThread = new Thread(() => println("Hi, Scala"))



  // 3 - methods ending in a : are RIGHT-ASSOCIATIVE
  // (evaluated from right to left)
  val aList = List(1,2,3)
  val aPrependedList: List[Int] = 0 :: aList // is the same as aList.::(0) - isn't 0.::(aList)
  val aBigList: List[Int] = 0 :: 1 :: 2 :: List(3,4) // List(3,4).::(2).::(1).::(0)

  class MyStream[T] {
    infix def -->:(value: T): MyStream[T] = this // impl not important
  }

  val myStream: MyStream[Int] = 1 -->: 2 -->: 3 -->: 4 -->: new MyStream[Int]


  // 4 - multi-word identifiers
  class Talker(name: String) {
    infix def `and then said`(gossip: String): Unit = println(s"$name said $gossip")
  }

  val daniel = new Talker("Daniel")
  val danielsStatement: Unit = daniel `and then said` "I love Scala"

  // example: HTTP libraries
  object `Content-Type` {
    val `application/json` = "application/JSON"
  }


  // 5 - infix types
  import scala.annotation.targetName

  @targetName("Arrow") // alternative name for the type is "Arrow", for more readable bytecode + Java interop
  infix class -->[A, B]

  val compositeType: Int --> String = new -->[Int, String] // the type is Int --> String, same as -->[Int, String] but infix


  // 6 - update()
  val anArray: Array[Int] = Array(1,2,3,4)
  anArray.update(2, 45)
  anArray(2) = 45 // same as above (works for any other data structures having `update`method that takes two parameters)


  // 7 - mutable fields
  class Mutable {
    private var internalMember: Int = 0

    def member: Int = internalMember // "getter"
    def member_=(value: Int): Unit = // "setter" (note the suffix `_=`)
      internalMember = value
  }

  val aMutableContainer = new Mutable
  aMutableContainer.member = 42 // aMutableContainer.member_=(42)


  // 8 - variable arguments (varargs)
  def methodWithVarargs(args: Int*): Int = { // Int* has the same api as Seq[Int]
    // return the number of arguments supplied
    args.length
  }

  val callWithZeroArgs: Int = methodWithVarargs()
  val callWithOneArgs: Int = methodWithVarargs(78)
  val callWithTwoArgs: Int = methodWithVarargs(12, 34)

  val aCollection = List(1,2,3,4)
  val callWithDynamicArgs: Int = methodWithVarargs(aCollection*) // "spread operator", to expand the collection


  def main(args: Array[String]): Unit = {

  }
}
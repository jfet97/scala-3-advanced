package com.rockthejvm.part2afp

object LazyEvaluation {

  lazy val x: Int = {
    println("Hello")
    42
  }

  // lazy delays the evaluation of a value until the first use
  // evaluation occurs once for all


  /*
    Example 1: call by need (call by name + lazy values)
   */
  def byNameMethod(n: => Int): Int =
    n + n + n + 1

  // call by name
  def retrieveMagicValue() = {
    println("waiting...")
    Thread.sleep(1000)
    42
  }

  def demoByName(): Unit = {
    println(byNameMethod(retrieveMagicValue()))
    // retrieveMagicValue() + retrieveMagicValue() + retrieveMagicValue() + 1
    // retrieveMagicValue() is evaluated each time
  }

  // call by need
  def byNeedMethod(n: => Int): Int = {
    // n will be evaluated only if needed
    // lazy val lazyN makes it evaluated only once

    lazy val lazyN = n // memoization <--
    lazyN + lazyN + lazyN + 1
  }

  def demoByNeed(): Unit = {
    println(byNeedMethod(retrieveMagicValue()))
    // retrieveMagicValue() + retrieveMagicValue() + retrieveMagicValue() + 1
    // retrieveMagicValue() is evaluated only one
  }


  /*
    Example 2: withFilter
   */
  def lessThan30(i: Int): Boolean = {
    println(s"$i is less than 30?")
    i < 30
  }

  def greaterThan20(i: Int): Boolean = {
    println(s"$i is greater than 20?")
    i > 20
  }

  val numbers = List(1, 25, 40, 5, 23)

  def demoFilter(): Unit = {
    val lt30 = numbers.filter(lessThan30)
    val gt20 = lt30.filter(greaterThan20)
    println(gt20)
  }

  def demoWithFilter(): Unit = {
    val lt30 = numbers.withFilter(lessThan30) // not evaluated actually
    val gt20 = lt30.withFilter(greaterThan20) // not evaluated actually
    println(gt20.map(identity)) // now evaluate the gt20 WithFilter into a List
  }

  def demoForComprehension(): Unit = {
    val forComp = for {
      n <- numbers if lessThan30(n) && greaterThan20(n)
    } yield n
    println(forComp)
  }


  def main(args: Array[String]): Unit = {
    println(x)
    println(x) // x is evaluated only once

    demoByName() // three side effects
    demoByNeed() // one side effect

    println("")

    println("demoFilter:")
    demoFilter()

    println("")
    // notice the different order of prints
    // because further elements are not evaluated until necessary

    println("demoWithFilter:")
    demoWithFilter()

    println("")

    println("demoForComprehension:")
    demoForComprehension()

    println("")
  }
}
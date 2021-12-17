package com.rockthejvm.part2afp

object CurryingPAFs {

  // currying
  val superAdder: Int => Int => Int =
    x => y => x + y

  val add3: Int => Int = superAdder(3) // y => 3 + y
  val eight: Int = add3(5) // 8
  val eight_v2: Int = superAdder(3)(5)

  // curried methods
  def curriedAdder(x: Int)(y: Int): Int =
    x + y


  // ACHTUNG: methods != function values
  // however the scala compilers allows a faster conversion

  // converting methods to function values = eta-expansion
  val add4: Int => Int = curriedAdder(4)
  val nine: Int = add4(5) // 9

  def increment(x: Int): Int = x + 1
  val aList = List(1,2,3)
  val anIncrementedList: List[Int] = aList.map(increment) // eta-expansion of the increment method

  // underscores are powerful: allow you to decide the shapes of lambdas obtained from methods (partial application)
  def concatenator(a: String, b: String, c: String): String = a + b + c
  val insertName: String => String = concatenator(
    "Hello, my name is ",
    _, // _: String,
    ", I'm going to show you a nice Scala trick."
  ) // s => concatenator("...", s, "...")

  val danielsGreeting: String = insertName("Daniel") // concatenator("...", "Daniel", "...")
  val fillInTheBlanks: (String, String) => String = concatenator(_, "Daniel", _) // (x, y) => concatenator(x, "Daniel", y)
  val danielsGreeting_v2: String = fillInTheBlanks("Hi, ", " how are you?")


  // methods vs functions + by-name vs 0-lambdas

  def byName(n: => Int): Int = n + 1
  def byLambda(f: () => Int): Int = f() + 1

  def method: Int = 42
  def parenMethod(): Int = 42 // use to signal that side-effects are involved

  byName(23) // ok
  byName(method) // 43. eta-expanded? NO - method is lazily INVOKED here
  byName(parenMethod()) // 43
  // byName(parenMethod) // not ok in Scala 3
  byName((() => 42)()) // ok
  // byName(() => 42) // not ok in Scala 3

  // byLambda(23) // not ok
  // byLambda(method) // eta-expansion is NOT possible :(
  byLambda(parenMethod) // eta-expansion is done
  byLambda(() => 42)
  byLambda(() => parenMethod()) // ok (this is how the compiler does the eta-expansion)




  /**
   * Exercises
   */
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedMethod(x: Int)(y: Int) = x + y

  // 1 - obtain an add7 function: x => x + 7 out of these 3 definitions
  val add7 = (x: Int) => simpleAddFunction(x, 7)
  val add7_v2 = (x: Int) => simpleAddMethod(x, 7)
  val add7_v3 = (x: Int) => curriedMethod(7)(x)
  val add7_v4 = curriedMethod(7)
  val add7_v5 = simpleAddMethod(7, _)
  val add7_v6 = simpleAddMethod(_, 7)
  val add7_v7 = simpleAddFunction.curried(7) // <--

  // 2 - process a list of numbers and return their string representations under different formats
  // step 1: create a curried formatting method with a formatting string and a value
  def curriedFormatter(fmt: String)(number: Double): String = fmt.format(number)
  // step 2: process a list of numbers with various formats
  val piWith2Dec = "%8.6f".format(Math.PI) // 3.14
  val someDecimals = List(Math.PI, Math.E, 1, 9.8, 1.3e-12)


  def main(args: Array[String]): Unit = {
    println(danielsGreeting_v2)

    println(someDecimals.map(curriedFormatter("%4.2f")))
    println(someDecimals.map(curriedFormatter("%8.6f")))
    println(someDecimals.map(curriedFormatter("%16.14f")))

  }
}

package com.rockthejvm.part2afp

object PartialFunctions {

  val aFunction: Int => Int = x => x + 1

  val aFussyFunction: Int => Int = (x: Int) =>
    if (x == 1) 42
    else if (x == 2) 56
    else if (x == 5) 999
    else throw new RuntimeException("no suitable cases possible")

  // usually we can compact the above code using pattern matching:
  val aFussyFunction_v2: Int => Int = (x: Int) => x match {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }
  val aFussyFunction_v3: Int => Int = { // same as aFussyFunction_v2
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }

  // partial function: is a unary function where the domain
  // does not necessarily include all values of its type
  val aPartialFunction: PartialFunction[Int, Int] = { // same as x => x match { ... }
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }


  // utilities on partial functions
  val canCallOn37: Boolean = aPartialFunction.isDefinedAt(37)
  val liftedPF: Int => Option[Int] = aPartialFunction.lift // Int => Option[Int]

  val anotherPF: PartialFunction[Int, Int] = {
    case 45 => 86
  }
  val pfChain: PartialFunction[Int, Int] = aPartialFunction.orElse(anotherPF)

  // HOFs accept PFs as arguments
  val aList = List(1,2,3,4)

  val aChangedList: List[Int] = aList.map(pfChain)

  val aChangedList_v2: List[Int] = aList.map(x => x match {
    case 1 => 4
    case 2 => 3
    case 3 => 45
    case 4 => 67
  })

  val aChangedList_v3: List[Int] = aList.map({ // possible because PartialFunction[A, B] extends Function1[A, B]
    case 1 => 4
    case 2 => 3
    case 3 => 45
    case 4 => 67
  })

  val aChangedList_v4: List[Int] = aList.map {
    case 1 => 4
    case 2 => 3
    case 3 => 45
    case 4 => 67
  }

  case class Person(name: String, age: Int)
  val someKids = List(
    Person("Alice", 3),
    Person("Bobbie", 5),
    Person("Jane", 4)
  )

  // pattern matching of the argument of a lambda (technically is a PF because the default case is missing)
  val kidsGrowingUp: List[Person] = someKids.map {
    case Person(name, age) => Person(name, age + 1)
  }


  def main(args: Array[String]): Unit = {
    println(aPartialFunction(2))
    // println(aPartialFunction(33)) // throws MatchError
    println(liftedPF(5)) // Some(999)
    println(liftedPF(37)) // None
    println(pfChain(45))
  }
}
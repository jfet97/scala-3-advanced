package com.rockthejvm.part4context

import scala.annotation.tailrec

object ExtensionMethods {

  // to enhance existing types after they have been defined,
  // included the ones of the standard library

  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name, nice to meet you!"
  }

  // add to the String class a new method
  extension (string: String) // <- only one argument
    def greetAsPerson: String = Person(string).greet

  val danielGreeting = "Daniel".greetAsPerson


  // generic extension methods

  extension [A](list: List[A])
    def ends: (A, A) = (list.head, list.last)

  val aList = List(1,2,3,4)
  val firstLast = aList.ends


  // reason: make APIs very expensive
  // reason: enhance certain types with new capabilities
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  extension [A](list: List[A])
    def combineAll(using comb: Semigroup[A]): A =
      list.reduce(comb.combine)

  given intCombinator: Semigroup[Int] with
    override def combine(x: Int, y: Int): Int = x + y

  val firstSum = aList.combineAll // available because there is an intCombinator in scope
  val someStrings = List("I", "love", "Scala")
  // val stringSum = someStrings.combineAll // does not compile - no given Combinator[String] in scope


  // we can group extension methods together
  object newScope {
    extension [A](list: List[A]) {
      def ends: (A, A) = (list.head, list.last)

      def combineAll(using comb: Semigroup[A]): A =
        list.reduce(comb.combine)
    }

  }

  // we can call extension methods directly
  val firstLast_v2 = ends(aList) // same as aList.ends




  /**
   * Exercises
   * 1. Add an isPrime method to the Int type.
   *    You should be able to write 7.isPrime
   * 2. Add extensions to Tree:
   *    - map(f: A => B): Tree[B]
   *    - forall(predicate: A => Boolean): Boolean
   *    - sum => sum of all elements of the tree
   */

  // 1
  extension (num: Int) {
    def isPrime: Boolean = {
      @tailrec
      def inner(curr: Int): Boolean = {
        if curr <= 1 then false
        else if curr == 2 then true
        else if num % curr == 0 then false
        else inner(curr - 1)
      }

      inner(num / 2)
    }
  }

  // 2 "library code" = cannot change
  sealed abstract class Tree[A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  extension [A](tree: Tree[A]) {
    def map[B](projection: A => B): Tree[B] = tree match {
      case Leaf(a) => Leaf(projection(a))
      case Branch(lt, rt) => Branch(lt.map(projection), rt.map(projection))
    }

    def forall(predicate: A => Boolean): Boolean = tree match {
      case Leaf(a) => predicate(a)
      case Branch(lt, rt) => lt.forall(predicate) && rt.forall(predicate)
    }

    def combineAll(using combinator: Semigroup[A]): A = tree match {
      case Leaf(value) => value
      case Branch(left, right) => combinator.combine(left.combineAll, right.combineAll)
    }
  }

  extension (tree: Tree[Int]) {
    def sum: Int = tree match {
      case Leaf(value) => value
      case Branch(left, right) => left.sum + right.sum
    }
  }

  def main(args: Array[String]): Unit = {
    println(danielGreeting)
    println(firstLast)
    println(firstSum)


    println(7.isPrime) // true
    println(10.isPrime) // false

    val aTree: Tree[Int] = Branch(Branch(Leaf(3), Leaf(1)), Leaf(10))
    println(aTree.map(_ + 1))
    println(aTree.forall(_ % 2 == 0)) // false
    println(aTree.sum) // 14
    println(aTree.combineAll) // 14
  }
}

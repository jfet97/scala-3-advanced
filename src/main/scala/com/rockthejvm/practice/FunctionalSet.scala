package com.rockthejvm.practice

import scala.annotation.tailrec

// When overriding a concrete member, Scala requires the override keyword.
// It is optional when a subtype defines (“overrides”) an abstract member.
// Conversely, don’t use override unless you are actually overriding a member.

abstract class FSet[A] extends (A => Boolean) {
  // main api
  def contains(elem: A): Boolean
  def apply(elem: A): Boolean = contains(elem)

  infix def +(elem: A): FSet[A]
  infix def ++(anotherSet: FSet[A]): FSet[A] // union

  // classic methods
  def map[B](f: A => B): FSet[B]
  def flatMap[B](f: A => FSet[B]): FSet[B]
  def filter(predicate: A => Boolean): FSet[A]
  def foreach(f: A => Unit): Unit

  def isEmpty: Boolean

  // utilities
  infix def -(elem: A): FSet[A] // remove an element
  infix def --(anotherSet: FSet[A]): FSet[A] // difference
  infix def &(anotherSet: FSet[A]): FSet[A] // intersection

  // "negation" == all the elements of type A except the elements in this set
  // if A => Boolean is a Set, A => !Boolean is a Set as well
  // a Set not necessary has to store its elements, them can be described by a function
  // which predicate the property of the elements contained in the set
  def unary_! : FSet[A] = new PropertyBasedSet(x => !contains(x))
}

// example { x in N | x % 2 == 0 }
// property-based set (intensional) instead of "a list" containing all the elements (extensional)
class PropertyBasedSet[A](property: A => Boolean) extends FSet[A] {
  // main api
  def contains(elem: A): Boolean = property(elem)

  infix def +(elem: A): FSet[A] =
    new PropertyBasedSet(x => x == elem || property(x))
  infix def ++(anotherSet: FSet[A]): FSet[A] =
    new PropertyBasedSet(x => property(x) || anotherSet(x))

  // "classics"
  def map[B](f: A => B): FSet[B] =
    politelyFail()
  def flatMap[B](f: A => FSet[B]): FSet[B] =
    politelyFail()
  def filter(predicate: A => Boolean): FSet[A] =
    new PropertyBasedSet(x => property(x) && predicate(x))
  def foreach(f: A => Unit): Unit =
    politelyFail()


  def isEmpty = throw new RuntimeException("I don't know if this set is empty...")

  // utilities
  infix def -(elem: A): FSet[A] =
    filter(x => x != elem) // property(x) AND x not the same as the removed elem
  infix def --(anotherSet: FSet[A]): FSet[A] =
    filter(!anotherSet) // property(x) AND x contained in the complementary of anotherSet
  infix def &(anotherSet: FSet[A]): FSet[A] =
    filter(anotherSet) // property(x) AND x contained in anotherSet

  // extra utilities (internal)
  private def politelyFail() = throw new RuntimeException("I don't know if this set is iterable...")
}

case class Empty[A]() extends FSet[A] {
  // main api
  override def contains(elem: A): Boolean = false

  override infix def +(elem: A): FSet[A] = Cons(elem, this)
  override infix def ++(anotherSet: FSet[A]): FSet[A] = anotherSet

  // classic methods
  override def map[B](f: A => B): FSet[B] = Empty()
  override def flatMap[B](f: A => FSet[B]): FSet[B] = Empty()
  override def filter(predicate: A => Boolean): FSet[A] = this
  override def foreach(f: A => Unit): Unit = ()

  override def isEmpty: Boolean = true

  override infix def -(elem: A): FSet[A] = this
  override infix def --(anotherSet: FSet[A]): FSet[A] = this
  override infix def &(anotherSet: FSet[A]): FSet[A] = this
}

case class Cons[A](head: A, tail: FSet[A]) extends FSet[A] {
  // main api
  override def contains(elem: A): Boolean = elem == head || tail.contains(elem)

  override infix def +(elem: A): FSet[A] =
    if this.contains(elem) then this
    else Cons(elem, this)

  override infix def ++(anotherSet: FSet[A]): FSet[A] = anotherSet ++ tail + head // + head will add the head only if it is not already present

  // classic methods
  override def map[B](f: A => B): FSet[B] = tail.map(f) + f(head)
  override def flatMap[B](f: A => FSet[B]): FSet[B] = tail.flatMap(f) ++ f(head)
  override def filter(predicate: A => Boolean): FSet[A] = {
    val filteredTail = tail.filter(predicate)
    if predicate(head) then filteredTail + head
    else filteredTail
  }
  override def foreach(f: A => Unit): Unit =
    tail.foreach(f)
    f(head)

  override def isEmpty: Boolean = false

  override infix def -(elem: A): FSet[A] =
    if head == elem then tail
    else tail - elem + head // remove the element from the tail, than add the head back
  override infix def --(anotherSet: FSet[A]): FSet[A] = filter(!anotherSet(_)) // O(nˆ2)
  override infix def &(anotherSet: FSet[A]): FSet[A] = filter(anotherSet) // O(nˆ2)
}

object FSet {
  def apply[A](values: A*): FSet[A] = {
    @tailrec
    def buildSet(valuesSeq: Seq[A], acc: FSet[A] = Empty()): FSet[A] =
      import valuesSeq.{isEmpty as vsIsEmpty, tail as vsTail, head as vsHead}
      if vsIsEmpty then acc
      else buildSet(vsTail, acc + vsHead)

    buildSet(values)
  }
}

object FunctionalSetPlayground {
  def main(args: Array[String]): Unit = {

    val first5 = FSet(1,2,3,4,5)
    val someNumbers = FSet(4,5,6,7,8)
    println(first5.contains(5)) // true
    println(first5(6))          // false
    println((first5 + 10).contains(10)) // true
    println(first5.map(_ * 2).contains(10)) // true
    println(first5.map(_ % 2).contains(1))  // true
    println(first5.flatMap(x => FSet(x, x + 1)).contains(7)) // false

    println((first5 - 3).contains(3)) // false
    println((first5 -- someNumbers).contains(4)) // false
    println((first5 & someNumbers).contains(4)) // true

    println("")
    // sets are functions A => Boolean (and viceversa eheh)
    val aSet = Set(1,2,3)
    val aList = (1 to 10).toList
    println(aList.filter(aSet)) // calls `apply` which calls `contain`

    val naturals = new PropertyBasedSet[Int](_ => true) // same as !Empty[Int](), so !naturals is the same as Empty[Int]()
    println(naturals.contains(5237548)) // true
    println(!naturals.contains(0)) // false
    println((!naturals + 1 + 2 + 3).contains(3)) // true
    // println(!naturals.map(_ + 1)) // throws - map/flatMap/foreach will not work

  }
}

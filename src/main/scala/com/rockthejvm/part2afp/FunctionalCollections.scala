package com.rockthejvm.part2afp

object FunctionalCollections {

  // sets are functions A => Boolean
  val aSet: Set[String] = Set("I", "love", "Scala")
  val setContainsScala: Boolean = aSet("Scala") // true

  // Seq extends PartialFunction[Int, A]
  val aSeq: Seq[Int] = Seq(1,2,3,4)
  val anElement: Int = aSeq(2) // 3
  val aNonExistingElement: Int = aSeq(100) // throws an IndexOutOfBoundsException

  // Map[K,V] "extends" PartialFunction[K, V]
  val aPhonebook: Map[String, Int] = Map(
    "Alice" -> 123456,
    "Bob" -> 987654
  )
  val alicesPhone: Int = aPhonebook("Alice")
  // val danielsPhone = aPhonebook("Daniel") // throws a NoSuchElementException

  def main(args: Array[String]): Unit = {

  }
}
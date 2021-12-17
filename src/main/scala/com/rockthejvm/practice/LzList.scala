package com.rockthejvm.practice

import scala.annotation.tailrec

// Write a lazily evaluated, potentially INFINITE linked list
abstract class LzList[A] {
  def isEmpty: Boolean
  def head: A
  def tail: LzList[A]

  // utilities
  def #::(element: A): LzList[A] // prepending
  infix def ++(another: => LzList[A]): LzList[A]
  // => LzList[A] avoid the complete evaluation of `another` list
  // in the current (recursive) implementation

  // classics
  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): LzList[B]
  def flatMap[B](f: A => LzList[B]): LzList[B]
  def filter(predicate: A => Boolean): LzList[A]
  def withFilter(predicate: A => Boolean): LzList[A] = filter(predicate) // because LzList is lazy evaluated

  def take(n: Int): LzList[A] // takes the first n elements from this lazy list
  def takeAsList(n: Int): List[A] =
    take(n).toList

  // ACHTUNG: never ends (or breaks) if `this` is an infinite, lazy list
  def toList: List[A] = {
    @tailrec
    def toListAux(remaining: LzList[A], acc: List[A] = List()): List[A] =
      if (remaining.isEmpty) acc.reverse
      else toListAux(remaining.tail, remaining.head :: acc)

    toListAux(this)
  }
}

case class LzEmpty[A]() extends LzList[A] {
  def isEmpty: Boolean = true
  def head: A = throw new NoSuchElementException
  def tail: LzList[A] = throw new NoSuchElementException

  // utilities
  def #::(element: A): LzList[A] = new LzCons(element, this)
  infix def ++(another: => LzList[A]): LzList[A] = another

  // classics
  def foreach(f: A => Unit): Unit = ()
  def map[B](f: A => B): LzList[B] = LzEmpty()
  def flatMap[B](f: A => LzList[B]): LzList[B] = LzEmpty()
  def filter(predicate: A => Boolean): LzList[A] = this

  def take(n: Int): LzList[A] =
    if (n == 0) this
    else throw new RuntimeException(s"Cannot take $n elements from an empty lazy list.")
}

// cannot be a case class because case classes don't support by name constructor arguments
// given that them becomes fields
class LzCons[A](hd: => A, tl: => LzList[A]) extends LzList[A] {

  def isEmpty: Boolean = false

  // use call by need, to evaluate hd and tl only once when will be necessary
  override lazy val head: A = hd
  override lazy val tail: LzList[A] = tl

  // utilities
  def #::(element: A): LzList[A] = new LzCons[A](element, this)
  infix def ++(another: => LzList[A]): LzList[A] =
    new LzCons(head, tail ++ another) // both arguments of LzCons are lazy evaluated

  // cannot preserve lazy evaluation
  def foreach(f: A => Unit): Unit = {
    def inner(lzList: LzList[A]): Unit =
      if lzList.isEmpty then ()
      else
        f(lzList.head)
        inner(lzList.tail)



    inner(this)
  }

  def map[B](f: A => B): LzList[B] =
    new LzCons[B](f(head), tail.map(f)) // preserves lazy evaluation

  def flatMap[B](f: A => LzList[B]): LzList[B] =
    // evaluation of head is forced
    // we need to delay the evaluation of tail.flatMap(f), as an argument of the operator ++, because
    // it is a recursive operation that may ends up with a stack overflow if `this` list is infinite
    f(head) ++ tail.flatMap(f)

  def filter(predicate: A => Boolean): LzList[A] =
    // we are forced to evaluate the head :(
    // BAD if `this` is an infinite list and no element satisfies the property
    if predicate(head) then new LzCons(head, tail.filter(predicate)) // preserves lazy eval
    else tail.filter(predicate)

  // has to evaluate the first n elements of the list
  def take(n: Int): LzList[A] = {
    if (n <= 0) LzEmpty()
    else if (n == 1) new LzCons(head, LzEmpty()) // both arguments of LzCons are lazy evaluated
    else new LzCons(head, tail.take(n - 1)) // preserves lazy eval
    // both arguments of LzCons are lazy evaluated
  }
}

object LzList {
  def empty[A]: LzList[A] = LzEmpty()

  def generate[A](start: A)(generator: A => A): LzList[A] =
    new LzCons(start, LzList.generate(generator(start))(generator)) // both arguments of LzCons are lazy evaluated
    // example: val naturals = LzList.generate(1)(_ + 1)

  // LzEmpty() as start value would force us to return, as accumulator, a LzEmpty[A] instance instead of a LzList[A]
  def from[A](list: List[A]): LzList[A] = list.reverse.foldLeft(LzList.empty)((acc, curr) => new LzCons(curr, acc))

  def apply[A](values: A*) = LzList.from(values.toList)

  def fibonacci: LzList[BigInt] = {
    def fibo(first: BigInt, second: BigInt): LzList[BigInt] =
      new LzCons[BigInt](first, fibo(second, first + second))

    fibo(1, 1)
  }

  def eratosthenes: LzList[Int] = {

    val naturals = LzList.generate(0)(_ + 1)

    // easy
    def isPrime(n: Int): Boolean = {
      @tailrec
      def inner(div: Int): Boolean = {
        if div <= 1 then true
        else if n % div == 0 then false
        else inner(div - 1)
      }

      if n <= 1 then false
      else inner(n / 2)
    }
    val primes = naturals.filter(isPrime)

    // clever
    def sieve(ns: LzList[Int]): LzList[Int] =
      new LzCons[Int](ns.head, sieve(ns.tail.filter(_ % ns.head != 0)))

    val primes_v2 = sieve(naturals.tail.tail)

    primes_v2
  }
}

/**
Exercises:
      1. Lazy list of Fibonacci numbers
        1,2,3,5,8,13,21,34 ...
      2. Infinite list of prime numbers
        - filter naturals with isPrime (easy)
        - Eratosthenes' sieve
        [2,3,4,5,6,7,8,9,10,11,12,13,14,15,...
        [2,3,5,7,9,11,13,15,17,...
        [2,3,5,7,11,13,17,19,23,25,29,...
        [2,3,5,7,11,13,17,19,23,29,...
        sieve([2,3,4,5,6...]) =
        2 #:: sieve([3,4,5,6...].filter(_ % 2 != 0))
        2 #:: sieve([3,5,7,9,...])
        2 #:: 3 #:: sieve([5,7,9,11,...].filter(_ % 3 != 0))
        ... ad infinitum.
 */

object LzListPlayground {
  def main(args: Array[String]): Unit = {
    val naturals = LzList.generate(1)(n => n + 1)

    // tail evaluated on demand
    println(naturals.head) // 1
    println(naturals.tail.head) // 2
    println(naturals.tail.tail.head) // 3

    val first50k = naturals.take(50000)
    // first50k.foreach(println) // is ok
    val first50kList = first50k.toList

    println(naturals.map(_ * 2).takeAsList(100))
    println(naturals.flatMap(x => LzList(x, x + 1)).takeAsList(100))
    println(naturals.filter(_ < 10).takeAsList(9))
    // println(naturals.filter(_ < 10).takeAsList(10)) // crash with SO or infinite recursion

    // for-comprehension uses withFilter that implies lazy evaluation
    val combinationsLazy: LzList[String] = for {
      number <- naturals
      string <- LzList("black", "white")
    } yield s"$number-$string"
    println(combinationsLazy.takeAsList(20))


    // fibonacci
    val fibos = LzList.fibonacci
    println(fibos.takeAsList(100))

    // primes
    val primes = LzList.eratosthenes
    println(primes.takeAsList(10000))

  }
}

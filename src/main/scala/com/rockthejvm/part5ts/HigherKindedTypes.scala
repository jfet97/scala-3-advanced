package com.rockthejvm.part5ts
import scala.util.Try

object HigherKindedTypes {

  // generic types where the type arguments are themselves generic

  class HigherKindedType[F[_]] // hkt
  class HigherKindedType_v2[F[_], G[_], T] // hkt
  val hkExample = new HigherKindedType[List]
  val hkExample_v2 = new HigherKindedType_v2[List, Option, Boolean]
  // can use hkts for methods as well



  // why: abstract libraries, e.g. Cats
  // example: Functor
  val aList = List(1,2,3)
  val anOption = Option(2)
  val aTry = Try(42)

  val anIncrementedList = aList.map(_ + 1) // List(2,3,4)
  val anIncrementedOption = anOption.map(_ + 1) // Some(3)
  val anIncrementedTry = aTry.map(_ + 1) // Success(43)

  // "duplicated" APIs
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  def do10xTry(theTry: Try[Int]): Try[Int] = theTry.map(_ * 10)

  // DRY principle
  // step 1: TC definition
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  // step 2: TC instances
  given listFunctor: Functor[List] with
    override def map[A, B](list: List[A])(f: A => B): List[B] = list.map(f)

  // step 3: generic "user-facing" API
  def do10x[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] =
    functor.map(container)(_ * 10)

  // if you create TC instances for Option and Try, the do10x method will work on Option and Try too

  // step 4: extension methods: if there is a Functor[F] instance in scope, the hkt F[_] gains a map method
  extension [F[_], A](container: F[A])(using functor: Functor[F])
    def map[B](f: A => B): F[B] = functor.map(container)(f)

  // if there is a Functor instance in scope we know we can call map directly on the container because of the extension method
  def do10x_v2[F[_] : Functor](container: F[Int]): F[Int] =
    container.map(_ * 10) // map is an extension method



  /**
   * Exercise: implement a new type class on the same structure as Functor.
   * In the general API, must use for-comprehensions (=> Monad because we need flatmap).
   */

  // 1 - TC definition
  trait Monad[F[_]] extends Functor[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  // 2 - TC instance(s)
  given listMonad: Monad[List] with {
    override def map[A, B](list: List[A])(f: A => B) = list.map(f)
    override def flatMap[A, B](list: List[A])(f: A => List[B]) = list.flatMap(f)
  }

  given optionMonad: Monad[Option] with {
    override def map[A, B](option: Option[A])(f: A => B) = option.map(f)
    override def flatMap[A, B](option: Option[A])(f: A => Option[B]) = option.flatMap(f)
  }

  given tryMonad: Monad[Try] with {
    override def map[A, B](t: Try[A])(f: A => B) = t.map(f)
    override def flatMap[A, B](t: Try[A])(f: A => Try[B]) = t.flatMap(f)
  }

  // 3 - "user-facing" API
  def combine[F[_], A, B](fa: F[A], fb: F[B])(using monadIntance: Monad[F]): F[(A, B)] =
    monadIntance.flatMap(fa)(a => monadIntance.map(fb)(b => (a,b)))

  // 4 - extension
  extension [F[_], A](container: F[A])(using magic: Monad[F])
    def flatMap[B](f: A => F[B]): F[B] = magic.flatMap(container)(f)


  def combine_v2[F[_] : Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for {
      a <- fa // flatmap
      b <- fb // map
    } yield (a, b)




  def main(args: Array[String]): Unit = {
    println(do10x(List(1,2,3)))
  }
}

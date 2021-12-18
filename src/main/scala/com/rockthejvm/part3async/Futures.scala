package com.rockthejvm.part3async

import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.{Try, Success, Failure}
import scala.util.Random
import scala.concurrent.duration.*

object Futures {

  // a computation, evaluated on some other thread,
  // that will complete at same point in the future


  // thread pool (Java specific)
  val executor = Executors.newFixedThreadPool(4)

  // thread pool (Scala-specific, to execute futures) that wraps the Java one
  given executionContext: ExecutionContext = ExecutionContext.fromExecutorService(executor)


  // INTRO

  def calculateMeaningOfLife(): Int = {
    // simulate long compute
    Thread.sleep(1000)
    42
  }

  // call by name => computation is delayed
  val aFuture: Future[Int] = Future.apply(calculateMeaningOfLife()) // given execution context automagically passed

  // inspect the value of the future right now
  // the computation may or may not have completed -> Option
  // if the computation has completed it can be a success or an error
  val futureInstantResult: Option[Try[Int]] = aFuture.value
  println(futureInstantResult)

  // callbacks (evaluated by unknown thread)
  aFuture.onComplete {
    case Success(value) => println(s"I've completed with the meaning of life: $value")
    case Failure(e) => println(s"My async computation failed: $e")
  } // given execution context automagically passed


  // FUNCTIONAL COMPOSITION

  /*
    Functional Programming on Futures
    Motivation: onComplete is a hassle
   */
  case class Profile(id: String, name: String) {
    def sendMessage(anotherProfile: Profile, message: String) =
      println(s"${this.name} sending message to ${anotherProfile.name}: $message")
  }

  object SocialNetwork {
    // "database"
    val names = Map(
      "rtjvm.id.1-daniel" -> "Daniel",
      "rtjvm.id.2-jane" -> "Jane",
      "rtjvm.id.3-mark" -> "Mark",
    )

    // friends "database"
    val friends = Map(
      "rtjvm.id.2-jane" -> "rtjvm.id.3-mark"
    )

    val random = new Random()

    // "API"
    def fetchProfile(id: String): Future[Profile] = Future {
      // fetch something from the database
      Thread.sleep(random.nextInt(300)) // simulate the time delay
      Profile(id, names(id))
    }

    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
      Thread.sleep(random.nextInt(400))
      val bestFriendId = friends(profile.id)
      Profile(bestFriendId, names(bestFriendId))
    }
  }

  // problem: sending a message to my best friend

  def sendMessageToBestFriend(accountId: String, message: String): Unit = {
    // 1 - call fetchProfile
    // 2 - call fetchBestFriend
    // 3 - call profile.sendMessage(bestFriend)

    val profileFuture = SocialNetwork.fetchProfile(accountId)
    profileFuture.onComplete {
      case Success(profile) => // "code block"
        val friendProfileFuture = SocialNetwork.fetchBestFriend(profile)
        friendProfileFuture.onComplete {
          case Success(friendProfile) => profile.sendMessage(friendProfile, message)
          case Failure(e) => e.printStackTrace()
        }
      case Failure(ex) => ex.printStackTrace()
    }
  } // onComplete is such a pain - callback hell!

  // fp composition
  val janeProfileFuture = SocialNetwork.fetchProfile("rtjvm.id.2-jane")
  val janeFuture: Future[String] = janeProfileFuture.map(profile => profile.name) // map transforms value contained inside, ASYNCHRONOUSLY
  val janesBestFriend: Future[Profile] = janeProfileFuture.flatMap(profile => SocialNetwork.fetchBestFriend(profile))
  val janesBestFriendFilter: Future[Profile] = janesBestFriend.filter(profile => profile.name.startsWith("Z"))

  def sendMessageToBestFriend_v2(accountId: String, message: String): Unit = {
    val profileFuture = SocialNetwork.fetchProfile(accountId)

    val action = profileFuture.flatMap { profile =>
      SocialNetwork.fetchBestFriend(profile).map { bestFriend =>
        profile.sendMessage(bestFriend, message)
      }
    }

    // action // because the return type is forced to be Unit, everything put here will be ignored
  }

  // for comprehension
  def sendMessageToBestFriend_v3(accountId: String, message: String): Unit =
    for {
      profile <- SocialNetwork.fetchProfile(accountId)
      bestFriend <- SocialNetwork.fetchBestFriend(profile)
    } yield profile.sendMessage(bestFriend, message) // identical to v2


  // fallbacks

  // the map of recovering
  val profileNoMatterWhat: Future[Profile] = SocialNetwork.fetchProfile("unknown id").recover {
    case e: Throwable => Profile("rtjvm.id.0-dummy", "Forever alone")
  }

  // the flatmap of recovering
  val aFetchedProfileNoMatterWhat: Future[Profile] = SocialNetwork.fetchProfile("unknown id").recoverWith {
    case e: Throwable => SocialNetwork.fetchProfile("rtjvm.id.0-dummy") // if both fail, the second failure will be reported
  }

  // If both futures are failed, the resulting future holds the throwable object of the first future
  val fallBackProfile: Future[Profile] = SocialNetwork.fetchProfile("unknown id").fallbackTo(SocialNetwork.fetchProfile("rtjvm.id.0-dummy"))


  // BLOCK FOR A FUTURE

  // use only on extreme scenarios

  case class User(name: String)
  case class Transaction(sender: String, receiver: String, amount: Double, status: String)

  object BankingApp {
    // "APIs"
    def fetchUser(name: String): Future[User] = Future {
      // simulate some DB fetching
      Thread.sleep(500)
      User(name)
    }

    def createTransaction(user: User, merchantName: String, amount: Double): Future[Transaction] = Future {
      // simulate payment
      Thread.sleep(1000)
      Transaction(user.name, merchantName, amount, "SUCCESS")
    }

    // "external API"
    def purchase(username: String, item: String, merchantName: String, price: Double): String = { // <- does not return a Future
      /*
        1. fetch user
        2. create transaction
        3. WAIT for the txn to finish
       */
      val transactionStatusFuture: Future[String] = for {
        user <- fetchUser(username)
        transaction <- createTransaction(user, merchantName, price)
      } yield transaction.status

      // blocking call (block the current thread) - not recommended unless in VERY STRICT circumstances
      Await.result(transactionStatusFuture, 2.seconds) // throws TimeoutException if the future doesn't finish within 2s
    }
  }


  // PROMISES

  // a technique for controlling the completion of Futures

  def demoPromises(): Unit = {
    val promise = Promise[Int]()
    val futureInside: Future[Int] = promise.future

    // thread 1 - "consumer": monitor the future for completion
    futureInside.onComplete {
      case Success(value) => println(s"[consumer] I've just been completed with $value")
      case Failure(ex) => ex.printStackTrace()
    }

    // thread 2 - "producer"
    val producerThread = new Thread(() => {
      println("[producer] Crunching numbers...")
      Thread.sleep(1000)
      // "fulfil" the promise
      promise.success(42)
      println("[producer] I'm done.")
    })

    producerThread.start()
  }



  /**
  Exercises
    1) fulfil a future IMMEDIATELY with a value
    2) in sequence: make sure the first Future has been completed before returning the second
    3) first(fa, fb) => new Future with the value of the first Future to complete
    4) last(fa, fb) => new Future with the value of the LAST Future to complete
    5) retry an action returning a Future until a predicate holds true
   */

  // 1
  def completeImmediately[A](value: A): Future[A] = Future(value) // async completion as soon as possible
  def completeImmediately_v2[A](value: A): Future[A] = Future.successful(value) // synchronous completion

  // 2
  def inSequence[A, B](first: Future[A], second: Future[B]): Future[B] =
    first.flatMap(_ => second)

  // 3
  def first[A](f1: Future[A], f2: Future[A]): Future[A] = {
    val promise = Promise[A]()

    // complete throws an exception if the promise has already completed
    f1.onComplete(result1 => promise.tryComplete(result1))
    f2.onComplete(result2 => promise.tryComplete(result2))

    promise.future
  }

  // 4
  def last[A](f1: Future[A], f2: Future[A]): Future[A] = {
    val bothPromise = Promise[A]()
    val lastPromise = Promise[A]()

    def checkAndComplete(result: Try[A]): Unit =
      // if I cannot complete the bothPromise that means that the other
      // has completed before me
      if (!bothPromise.tryComplete(result))
        lastPromise.complete(result)

    f1.onComplete(checkAndComplete)
    f2.onComplete(checkAndComplete)

    lastPromise.future
  }

  def testFirstLast(): Unit = {
    // when a Future is created, it starts evaluating immediately
    // unless the lazy keyword is used
    lazy val fast = Future {
      Thread.sleep(100)
      1
    }
    lazy val slow = Future {
      Thread.sleep(200)
      2
    }
    first(fast, slow).foreach(result => println(s"FIRST: $result"))
    last(fast, slow).foreach(result => println(s"LAST: $result"))
  }


  // 5
  def retryUntil[A](action: () => Future[A], predicate: A => Boolean): Future[A] =
    action()
        // If the current future contains a value which satisfies the predicate,
      // // the new future will also hold that value. Otherwise, the resulting future will fail
      .filter(predicate)
      .recoverWith {
        case _ => retryUntil(action, predicate)
      }

  def testRetries(): Unit = {
    val random = new Random()

    val action = () => Future {
      Thread.sleep(100)
      val nextValue = random.nextInt(100)
      println(s"Generated $nextValue")
      nextValue
    }

    val predicate = (x: Int) => x < 10

    retryUntil(action, predicate).foreach(finalResult => println(s"Settled at $finalResult"))
  }



  def main(args: Array[String]): Unit = {

    // sendMessageToBestFriend_v3("rtjvm.id.2-jane", "Hey best friend, nice to talk to you again!")

    // println("purchasing...")
    // println(BankingApp.purchase("daniel-234", "shoes", "merchant- 987", 3.56))
    // println("purchase complete")

    // demoPromises()

    // testFirstLast()

    testRetries()

    Thread.sleep(2000)
    executor.shutdown()
  }
}


package com.rockthejvm.part3async

def runInParallel(): Unit = {
  var x = 0

  val thread1 = new Thread(() => {
    x = 1
  })

  val thread2 = new Thread(() => {
    x = 2
  })

  thread1.start()
  thread2.start()
  println(x) // race condition
}


// synchronization
case class BankAccount(var amount: Int)

def buy(bankAccount: BankAccount, thing: String, price: Int): Unit = {
  bankAccount.amount -= price // same as bankAccount.amount = bankAccount.amount - price
  // there is a race condition on the reading process of the bankAccount.amount
}

def buySafe(bankAccount: BankAccount, thing: String, price: Int): Unit = {
  bankAccount.synchronized { // does not allow multiple threads to run the critical section at the same time
    bankAccount.amount -= price // critical section
  }
}

def demoBankingProblem(): Unit = {
  (1 to 100000).foreach { _ =>
    val account = BankAccount(50000)
    val thread1 = new Thread(() => buy(account, "shoes", 3000))
    val thread2 = new Thread(() => buy(account, "iPhone", 4000))
    val thread3 = new Thread(() => buy(account, "books", 5000))
    val thread4 = new Thread(() => buy(account, "food", 6000))
    thread1.start()
    thread2.start()
    thread3.start()
    thread4.start()
    thread1.join()
    thread2.join()
    thread3.join()
    thread4.join()
    if (account.amount != 32000) println(s"AHA! I've just broken the bank: ${account.amount}")
  }
}

/**
Exercises
    1 - create "inception threads"
      thread 1
        -> thread 2
            -> thread 3
                ....
      each thread prints "hello from thread $i"
      Print all messages IN REVERSE ORDER
    2 - What's the max/min value of x?
    3 - "sleep fallacy": what's the value of message?
 */

// 1 - inception threads
def inceptionThreads(maxThreads: Int, i: Int = 1): Thread = {
  new Thread(() => {
    if(i > maxThreads) ()
    else {
      val recThread = inceptionThreads(maxThreads, i + 1)
      recThread.start()
      recThread.join()
      println(s"Hello from thread $i")
    }
  })
}

// 2 - min max
def minMaxX(): Unit = {
  var x = 0
  val threads = (1 to 100).map(_ => new Thread(() => x += 1))
  threads.foreach(_.start())
  threads.foreach(_.join())
  println(x)
  // min: 1
  //   all threads read x = 0 at the same time
  //   all threads (in parallel) compute 0 + 1 = 1
  //   all threads try to write x = 1
  // max: 100
}

// 3 - sleep fallacy
def demoSleepFallacy(): Unit = {
  var message = ""
  val awesomeThread = new Thread(() => {
    Thread.sleep(1000)
    message = "Scala is awesome"
  })

  message = "Scala sucks"
  awesomeThread.start()
  Thread.sleep(1001)

  // solution: join the worker thread to guarantee message == "Scala is awesome"
  // awesomeThread.join()

  println(message)
}

object JVMConcurrencyProblems {
  def main(args: Array[String]): Unit = {
    // runInParallel()

    // demoBankingProblem()

    // inceptionThreads(100).start()

    minMaxX()
  }
}

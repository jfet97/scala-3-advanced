package com.rockthejvm.part3async

import java.util.concurrent.Executors
import scala.util.Try

object JVMConcurrencyIntro {

  def basicThreads(): Unit = {
    val runnable: Runnable = new Runnable {
      override def run(): Unit =
        println("running on some thread")
        Thread.sleep(1000)
        println("finished")
    }

    // threads on the JVM
    val aThread = new Thread(runnable)

    runnable.run() // nothing parallel right now

    aThread.start() // will run the runnable on some JVM thread
    // block until thread finishes
    aThread.join()
  }

  // order of operations is NOT guaranteed
  // different runs => different results
  def orderOfExecution(): Unit = {
    val threadHello = new Thread(() => (1 to 10).foreach(_ => println("hello")))
    val threadGoodbye = new Thread(() => (1 to 10).foreach(_ => println("goodbye")))

    threadHello.start()
    threadGoodbye.start()
  }

  // executors
  def demoExecutors(): Unit = {
    val threadPool = Executors.newFixedThreadPool(4)

    // submit (non blocking calls)

    threadPool.execute(() => println("something in the thread pool"))

    threadPool.execute { () =>
      Thread.sleep(1000)
      println("done after one second")
    }

    threadPool.execute { () =>
      Thread.sleep(2000)
      println("done after two second")
    }

    threadPool.execute { () =>
      Thread.sleep(750)
      println("almost done")
      Thread.sleep(750)
    }


    // shutdown the executor
    threadPool.shutdown()

    val errorMessage = Try(threadPool.execute(() => println("this should NOT appear"))).fold(_.getMessage, _ => "")
    println(errorMessage)

  }





  def main(args: Array[String]): Unit = {
    // basicThreads()

    // orderOfExecution()

    demoExecutors()
  }
}

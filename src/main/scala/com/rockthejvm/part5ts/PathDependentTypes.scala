package com.rockthejvm.part5ts

object PathDependentTypes {

  class Outer {
    class Inner
    object InnerObject
    type InnerType

    def process(arg: Inner) = println(arg)
    def processGeneral(arg: Outer#Inner) = println(arg)
  }


  val outer = new Outer
  val inner = new outer.Inner // outer.Inner is a separate TYPE = path-dependent type (depends on the instance)

  val outerA = new Outer
  val outerB = new Outer
  // val inner2: outerA.Inner = new outerB.Inner // path-dependent types are different

  val innerA = new outerA.Inner
  val innerB = new outerB.Inner

  // outerA.process(innerB) // type mismatch
  outer.process(inner) // ok


  // parent-type of all Inner types: Outer#Inner (it depends on the class, not on a specific instance)
  outerA.processGeneral(innerB) // correct, outerB.Inner <: Outer#Inner


  /*
    Why:
    - type-checking/type inference, e.g. Akka Streams: Flow[Int, Int, NotUsed]#Repr
    - type-level programming
   */

  // methods with dependent types: return a different COMPILE-TIME type depending on the argument
  // no need for generics
  trait Record {
    type Key
    def defaultValue: Key
  }

  class StringRecord extends Record {
    override type Key = String
    override def defaultValue: String = ""
  }

  class IntRecord extends Record {
    override type Key = Int
    override def defaultValue = 0
  }

  // user-facing API
  def getDefaultIdentifier(record: Record): record.Key = record.defaultValue

  val aString: String = getDefaultIdentifier(new StringRecord) // a string
  val anInt: Int = getDefaultIdentifier(new IntRecord) // an int, ok

  // functions with dependent types
  val getIdentifierFunc: Record => Record#Key = getDefaultIdentifier // eta-expansion


  def main(args: Array[String]): Unit = {

  }
}

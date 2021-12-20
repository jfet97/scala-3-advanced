package com.rockthejvm.part4context

object TypeClasses {

  /*
    Small library to serialize some data to a standard format (HTML)
   */


  // V1: the OO way

  trait HTMLWritable {
    def toHtml: String
  }

  case class User(name: String, age: Int, email: String) extends HTMLWritable {
    override def toHtml = s"<div>$name ($age yo) <a href=$email/></div>"
  }

  val bob = User("Bob", 43, "bob@rockthejvm.com")
  val bob2Html = bob.toHtml
  // same for other data structures that we want to serialize

  /*
    Drawbacks:
    - only available for the types we write (of our library)
    - can only provide one implementation
   */


  // V2: pattern matching

  object HTMLSerializerPM {
    def serializeToHtml(value: Any): String = value match {
      case User(name, age, email) => s"<div>$name ($age yo) <a href=$email/></div>"
      case _ => throw new IllegalArgumentException("data structure not supported")
    }
  }

  /*
    Drawbacks:
    - lost type safety
    - need to modify a single piece of code every time
    - still one implementation

    Bad because case classes may be not related at all, it's not the right way to address the problem
   */


  // V3 - type class

  // part 1 - type class definition: denotes the capability to serialize a type into an HTML string
  trait HTMLSerializer[T] {
    def serialize(value: T): String
  }

  // part 2 - type class instances for the supported types
  given userSerializer: HTMLSerializer[User] with {
    override def serialize(value: User) =  {
      val User(name, age, email) = value
      s"<div>$name ($age yo) <a href=$email/></div>"
    }
  }

  val bob2Html_v2 = userSerializer.serialize(bob)

  /*
    Benefits:
    - can define serializers for other types outside the "library"
    - multiple serializers for the same type, pick whichever you want
   */
  import java.util.Date
  given dateSerializer: HTMLSerializer[Date] with {
    override def serialize(date: Date) = s"<div>${date.toString()}</div>"
  }

  object SomeOtherSerializerFunctionality { // organize givens properly
    given partialUserSerializer: HTMLSerializer[User] with {
      override def serialize(user: User) = s"<div>${user.name}</div>"
    }
  }

  // part 3 - using the type class (user-facing API)
  object HTMLSerializer { // <- organized into the companion object of the typec lass
    def serialize[T](value: T)(using serializer: HTMLSerializer[T]): String =
      serializer.serialize(value)

    // it simply surfaces out whichever given instance we have in scope for that type class
    def apply[T](using serializer: HTMLSerializer[T]): HTMLSerializer[T] = serializer
  }

  val bob2Html_v3 = HTMLSerializer.serialize(bob) // we don't need to know the name of the serializer, we use an abstract API
  val bob2Html_v4 = HTMLSerializer[User].serialize(bob) // HTMLSerializer[User] calls the apply method

  // part 4 - brings bag the simplicity of the OOP approach
  object HTMLSyntax {
    // if there is a serializer for the type T, the toHTML method becomes available
    extension [T](value: T)
      def toHTML(using serializer: HTMLSerializer[T]): String = serializer.serialize(value)
  }

  import HTMLSyntax.*
  val bob2Html_v5 = bob.toHTML

  /*
    Cool!
    - extend functionality to new types that we want to support
    - flexibility to add TC instances in a different place than the definition of the TC
    - choose implementations (by importing the right givens)
    - super expressive! (via extension methods)
   */

  def main(args: Array[String]): Unit = {
    println(bob2Html)
    println(bob2Html_v2)
    println(bob2Html_v3)
    println(bob2Html_v4)
    println(bob2Html_v5)
  }
}

// addendum: your recipe for the type class pattern
object TypeClassTemplate {
  // 1 - type class definition
  trait MyTypeClass[T] {
    def action(value: T): String // can have multiple methods; adapt signatures to your needs
  }
  // 2 - type class instances
  given intInstance: MyTypeClass[Int] with
    override def action(value: Int) = value.toString
  // same for other types you want to support

  // 3 - user-facing API
  object MyTypeClass {
    // often similar to what the type class definition offers
    def action[T](value: T)(using instance: MyTypeClass[T]): String = instance.action(value)
    // often expose a method to retrieve the current given instance for a type (similar to summon)
    def apply[T](using instance: MyTypeClass[T]): MyTypeClass[T] = instance
  }

  // 4 - expressiveness through extension methods
  object MyTypeClassSyntax {
    extension [T](value: T)
      def action(using instance: MyTypeClass[T]): String =
        instance.action(value)
  }
}
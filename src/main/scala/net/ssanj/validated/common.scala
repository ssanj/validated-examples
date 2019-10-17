package net.ssanj.validated

object common {

  sealed trait PersonErrorType
  case object NameInvalid extends PersonErrorType
  case object AgeInvalid extends PersonErrorType
  case object EmailInvalid extends PersonErrorType

  final case class PersonError(value: String, errorType: PersonErrorType)
  final case class Person(name: Name, age: Age, email: Email)

  final case class Name(value: String)
  final case class Age(value: Int)
  final case class Email(value: String)

}
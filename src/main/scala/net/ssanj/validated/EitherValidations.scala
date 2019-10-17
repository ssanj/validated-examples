package net.ssanj.validated
import scala.util.Try

import common._

object EitherValidations {

  type ErrorOr[A] = Either[PersonError, A]

  private def validateName(name: String): ErrorOr[Name] = {
    if (name.headOption.exists(_.isUpper)) Right(Name(name))
    else Left(PersonError(s"Name is empty or does not start with an uppercase character: $name", NameInvalid))
  }
  
  private def validateAge(age: String): ErrorOr[Age] = for {
    numericAge <- Try(age.toInt).toEither.left.map(ex => PersonError(ex.getMessage, AgeInvalid))
    validAge <- { 
      if (numericAge <= 0 || numericAge > 120) Left(PersonError(s"Age must be a number between 1-120: ${numericAge}", AgeInvalid))
      else Right(numericAge)
    }
  } yield Age(validAge)

  private def validateEmail(email: String): ErrorOr[Email] = {
    if (email.isEmpty || !email.contains("@")) Left(PersonError(s"Email address is empty or does not contain an `@` symbol: $email", EmailInvalid))
    else Right(Email(email))
  }

  private def validatePerson(name: String, age: String, email: String): ErrorOr[Person] = for {
    validName <- validateName(name)
    validAge <- validateAge(age)
    validEmail <- validateEmail(email)
  } yield Person(validName, validAge, validEmail)


  def main(ages: Array[String]): Unit = {
    val validPerson = validatePerson("Benjamin Sisko", "50", "b.sisko@dsn.st")
    val invalidPerson = validatePerson("odo", "200", "odo.founder.net")

    println(s"valid person: ${validPerson}")
    println(s"invalid person: ${invalidPerson}")
  }
}
package net.ssanj.validated

import scala.util.Try
import cats.data.Validated
import cats.data.Validated._
import common._
import cats.implicits._

object ValidatedValidations {

  type AllErrorsOr[A] = Validated[PersonError, A]

  private def validateName(name: String): AllErrorsOr[Name] = {
    if (name.headOption.exists(_.isUpper)) Valid(Name(name))
    else Invalid(PersonError(s"Name is empty or does not start with an uppercase character: $name", NameInvalid))
  }


  private def validateName2(name: String): AllErrorsOr[Name] = {
    if (name.headOption.exists(_.isUpper)) Name(name).valid
    else PersonError(s"Name is empty or does not start with an uppercase character: $name", NameInvalid).invalid
  }

  private def validAgeV(numericAge: Int): AllErrorsOr[Int] = {
    if (numericAge <= 0 || numericAge > 120) PersonError(s"Age must be a number between 1-120: ${numericAge}", AgeInvalid).invalid
    else numericAge.valid
  }  
  
  private def validateAge(age: String): AllErrorsOr[Age] = {
    val numericAgeV: AllErrorsOr[Int] = Try(age.toInt).toEither.left.map(ex => PersonError(ex.getMessage, AgeInvalid)).toValidated
   
    numericAgeV.andThen(validAgeV).map(Age)
  }

 def validateAgeFromEither(age: String): AllErrorsOr[Age] = {

    val ageEither: Either[PersonError, Int] = Try(age.toInt).toEither.left.map(ex => PersonError(ex.getMessage, AgeInvalid))
    val validatedIntAge: Validated[PersonError, Int] = Validated.fromEither(ageEither)

    val numericAge: Int = ??? //we need some way to get the Int age out of validatedIntAge

    val validatedAge: Validated[PersonError, Age] = {
      if (numericAge <= 0 || numericAge > 120) Invalid(PersonError(s"Age must be a number between 1-120: ${numericAge}", AgeInvalid))
      else Valid(Age(numericAge))
    }

    validatedAge
  }  

  private def validateEmail(email: String): AllErrorsOr[Email] = {
    if (email.isEmpty || !email.contains("@")) PersonError(s"Email address is empty or does not contain an `@` symbol: $email", EmailInvalid).invalid
    else Email(email).valid
  }

  private def validatePerson(name: String, age: String, email: String): AllErrorsOr[Person] = {
    validateName(name).andThen(n => validateAge(age).andThen(a =>  validateEmail(email).map(e => Person(n, a, e))))
  }

  type StringErrorOr[A] = Validated[String, A]
 
  //Because String is a Semigroup we can use the Applicative/Apply functions.
  private def validatePerson3(name: String, age: String, email: String): StringErrorOr[Person] = {
    val validateName: StringErrorOr[Name] = Name("Bartosz").valid
    val validateAge: StringErrorOr[Age] = Age(60).valid
    val validateEmail: StringErrorOr[Email] = Email("bartosz@milewski.net").valid

    (validateName, validateAge, validateEmail).mapN(Person)
  }

  def main(ages: Array[String]): Unit = {
    // val validPerson = validatePerson("Benjamin Sisko", "50", "b.sisko@dsn.st")
    // val invalidPerson = validatePerson("odo", "200", "odo.founder.net")

    // val x = validateName("Benjamin Sisko").andThen(name => validateAge("50").andThen(age => validateEmail("b.sisko@dsn.st").andThen(email => Person(name, age, email).validNel)))
    // println(x)
    // println(s"valid person: ${validPerson}")
    // println(s"invalid person: ${invalidPerson}")
  }
}
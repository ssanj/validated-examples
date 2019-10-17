package net.ssanj.validated

import scala.util.Try
import cats.data.ValidatedNel
import cats.data.Validated._
import common._
import cats.implicits._
import cats.data.NonEmptyList

object ValidatedNelValidations {

  type AllErrorsOr[A] = ValidatedNel[PersonError, A]

  private def validateName(name: String): AllErrorsOr[Name] = {
    if (name.headOption.exists(_.isUpper)) Name(name).validNel
    else PersonError(s"Name is empty or does not start with an uppercase character: $name", NameInvalid).invalidNel
  }

  private def validateName2(name: String): AllErrorsOr[Name] = {
    if (name.headOption.exists(_.isUpper)) Valid(Name(name))
    else Invalid(NonEmptyList.of(PersonError(s"Name is empty or does not start with an uppercase character: $name", NameInvalid)))
  }

  private def validAgeV(numericAge: Int): AllErrorsOr[Int] = {
    if (numericAge <= 0 || numericAge >= 120) PersonError(s"Age must be a number between 1-120: ${numericAge}", AgeInvalid).invalidNel
    else numericAge.validNel
  }  
  
  private def validateAge(age: String): AllErrorsOr[Age] = {
    val numericAgeV: AllErrorsOr[Int] = Try(age.toInt).toEither.left.map(ex => PersonError(ex.getMessage, AgeInvalid)).toValidatedNel
   
    numericAgeV.andThen(validAgeV).map(Age)
  }

  private def validateEmail(email: String): AllErrorsOr[Email] = {
    if (email.isEmpty || !email.contains("@")) PersonError(s"Email address is empty or does not contain an `@` symbol: $email", EmailInvalid).invalidNel
    else Email(email).validNel
  }

  private def validatePerson(name: String, age: String, email: String): AllErrorsOr[Person] = {
    (validateName(name), validateAge(age), validateEmail(email)).mapN(Person)
  }

  private def validatePersonWithAndThen(name: String, age: String, email: String): AllErrorsOr[Person] = {
    validateName(name).andThen(validName => 
      validateAge(age).andThen(validAge => 
        validateEmail(email).map(validEmail => 
          Person(validName, validAge, validEmail)))) //ValidatedNel[PersonError, Person]
  }

  def validateNonEmptyName(nameString: String): AllErrorsOr[String] = 
    if (nameString.nonEmpty) nameString.validNel else PersonError(s"Name is empty", NameInvalid).invalidNel

  def validateStartsWithUpper(nameString: String): AllErrorsOr[String] = 
   if (nameString.headOption.exists(_.isUpper)) nameString.validNel else PersonError(s"$nameString does not start with an uppercase character", NameInvalid).invalidNel

  private def validateNameWithProduct(name: String): AllErrorsOr[Name] = {
    // validateNonEmptyName(name).andThen(_ => validateStartsWithUpper(name)).map(Name)
    validateNonEmptyName(name) productR validateStartsWithUpper(name) map Name
  }

  def main(ages: Array[String]): Unit = {
    val validPerson = validatePerson("Benjamin Sisko", "50", "b.sisko@dsn.st")
    val invalidPerson = validatePerson("odo", "200", "odo.founder.net")

    val x = validateName("Benjamin Sisko").andThen(name => validateAge("50").andThen(age => validateEmail("b.sisko@dsn.st").andThen(email => Person(name, age, email).validNel)))
    println(x)
    println(s"valid person: ${validPerson}")
    println(s"invalid person: ${invalidPerson}")
  }
}
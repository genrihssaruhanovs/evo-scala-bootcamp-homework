package evo.homework.error_handling
import cats.data.ValidatedNec
import cats.syntax.all._

object ErrorHandling {
  // Homework. Place the solution under `error_handling` package in your homework repository.
  //
  // 1. Model `CreditCard` class as an ADT (protect against invalid data as much as it makes sense).
  // 2. Add `ValidationError` cases (at least 5, may be more).
  // 3. Implement `validate` method to construct `CreditCard` instance from the supplied raw data.
  object Homework {

    case class CreditCard(name: String, number: String, expirationDate: String)

    sealed trait ValidationError
    object ValidationError {
      final case object NameHasSpecialCharacters extends ValidationError {
        override def toString: String = "Username cannot contain special characters"
      }
      final case object CardNumberIsNotNumeric extends ValidationError {
        override def toString: String = "Card number can only contain digits"
      }
      final case object CardNumberLengthMismatch extends ValidationError {
        override def toString: String = "Card number length must be precisely "
      }
    }

    object CreditCardValidator {

      type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

      def validate(
                    name: String,
                    number: String,
                    expirationDate: String,
                    securityCode: String,
                  ): AllErrorsOr[CreditCard] = ???
    }
  }
}

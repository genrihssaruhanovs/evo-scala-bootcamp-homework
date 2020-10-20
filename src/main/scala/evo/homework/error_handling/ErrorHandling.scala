package evo.homework.error_handling

import cats.data.ValidatedNec
import cats.syntax.all._
import java.time.YearMonth
import java.time.format.DateTimeFormatter

import scala.util.Try

object ErrorHandling {

  object Homework {
    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    case class CreditCard(name: String, number: String, expirationDate: YearMonth, securityCode: String)

    sealed trait ValidationError

    object ValidationError {

      final case object NameHasSpecialCharacters extends ValidationError {
        override def toString: String = "Username cannot contain special characters"
      }

      final case object CardNumberIsNotNumeric extends ValidationError {
        override def toString: String = "Card number can only contain numbers"
      }

      final case object CardNumberLengthMismatch extends ValidationError {
        override def toString: String = "Card number length must be precisely 16 digits"
      }

      final case object SecurityCodeIsNotNumeric extends ValidationError {
        override def toString: String = "Security code must only contain numbers"
      }

      final case object SecurityCodeLengthMismatch extends ValidationError {
        override def toString: String = "Security code must be precisely 3 digits"
      }

      final case object DateFormatMismatch extends ValidationError {
        override def toString: String = "Date format must be MM/yy"
      }

      final case object CreditCardIsExpired extends ValidationError {
        override def toString: String = "Credit card expiration date is before today's date"
      }
    }

    object CreditCardValidator {

      import ValidationError._

      private def validateName(name: String): AllErrorsOr[String] = {
        if (name.matches("^[a-z A-Z]+$")) name.validNec
        else NameHasSpecialCharacters.invalidNec
      }

      private def validateNumber(number: String): AllErrorsOr[String] = {
        def validateIsCardNumeric: AllErrorsOr[String] = if (number.forall(_.isDigit)) number.validNec else CardNumberIsNotNumeric.invalidNec

        def validateNumberLength: AllErrorsOr[String] = if (number.length == 16) number.validNec else CardNumberLengthMismatch.invalidNec

        validateIsCardNumeric *> validateNumberLength
      }

      private def validateExpirationDate(date: String): AllErrorsOr[YearMonth] = {
        def validateIsDate: AllErrorsOr[YearMonth] = {
          val dateTimeFormatter = DateTimeFormatter.ofPattern("MM/yy")
          Try(YearMonth.parse(date, dateTimeFormatter)).toOption match {
            case Some(convertedDate) => convertedDate.validNec
            case None => DateFormatMismatch.invalidNec
          }
        }

        def validateIsNotExpired(date: YearMonth): AllErrorsOr[YearMonth] = {
          if (date.isBefore(YearMonth.now())) CreditCardIsExpired.invalidNec else date.validNec
        }

        validateIsDate andThen validateIsNotExpired
      }

      private def validateSecurityCode(securityCode: String): AllErrorsOr[String] = {
        def validateIsSecurityCodeNumeric: AllErrorsOr[String] = if (securityCode.forall(_.isDigit)) securityCode.validNec else SecurityCodeIsNotNumeric.invalidNec

        def validateSecurityCodeLength: AllErrorsOr[String] = if (securityCode.length == 3) securityCode.validNec else SecurityCodeLengthMismatch.invalidNec

        validateIsSecurityCodeNumeric *> validateSecurityCodeLength
      }

      def validate(
                    name: String,
                    number: String,
                    expirationDate: String,
                    securityCode: String,
                  ): AllErrorsOr[CreditCard] = (validateName(name), validateNumber(number), validateExpirationDate(expirationDate), validateSecurityCode(securityCode)).mapN(CreditCard)
    }

  }

}

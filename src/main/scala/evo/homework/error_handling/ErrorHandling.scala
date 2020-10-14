package evo.homework.error_handling

import java.text.SimpleDateFormat

import cats.data.ValidatedNec
import cats.syntax.all._
import java.util.Date

import scala.util.Try

object ErrorHandling {

  object Homework {
    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

//number and CVV can have 0 in the beginning, therefore cannot be Numeric
    type cardNumber = String
    type securityCode = String

    case class CreditCard(name: String, number: cardNumber, expirationDate: Date, securityCode: securityCode)

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
        override def toString: String = "Date format must be dd.MM.yyyy or yyyyMMdd or dd/MM/yyyy"
      }

      final case object CreditCardIsExpired extends ValidationError {
        override def toString: String = "Credit card expiration date is before today's date"
      }

    }

    object CreditCardValidator {

      import ValidationError._

      private def validateName(name: String): AllErrorsOr[String] = {
        if (name.matches("^[a-zA-Z]+$")) name.validNec
        else NameHasSpecialCharacters.invalidNec
      }

      private def validateNumber(number: String): AllErrorsOr[cardNumber] = {
        def validateIsCardNumeric: AllErrorsOr[cardNumber] = if (number.forall(_.isDigit)) number.validNec else CardNumberIsNotNumeric.invalidNec

        def validateNumberLength: AllErrorsOr[cardNumber] = if (number.length == 16) number.validNec else CardNumberLengthMismatch.invalidNec

        validateIsCardNumeric *> validateNumberLength
      }

      private def validateExpirationDate(date: String): AllErrorsOr[Date] = {
        def validateIsDate: AllErrorsOr[Date] = {
          Try(new SimpleDateFormat("dd.MM.yyyy").parse(date)).toOption match {
            case Some(convertedDate) => convertedDate.validNec
            case None => Try(new SimpleDateFormat("dd/MM/yyyy").parse(date)).toOption match {
              case Some(convertedDate) => convertedDate.validNec
              case None => Try(new SimpleDateFormat("yyyyMMdd").parse(date)).toOption match {
                case Some(convertedDate) => convertedDate.validNec
                case None => DateFormatMismatch.invalidNec
              }
            }
          }
        }

        def validateIsNotExpired(date: Date): AllErrorsOr[Date] = {
          if (date.before(new Date())) CreditCardIsExpired.invalidNec else date.validNec
        }

        validateIsDate andThen validateIsNotExpired
      }

      private def validateSecurityCode(securityCode: String): AllErrorsOr[securityCode] = {
        def validateIsSecurityCodeNumeric: AllErrorsOr[securityCode] = if (securityCode.forall(_.isDigit)) securityCode.validNec else SecurityCodeIsNotNumeric.invalidNec

        def validateSecurityCodeLength: AllErrorsOr[securityCode] = if (securityCode.length == 3) securityCode.validNec else SecurityCodeLengthMismatch.invalidNec

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

package evo.homework.error_handling
import cats.syntax.all._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Assertion
import evo.homework.error_handling.ErrorHandling.Homework._
import java.time.YearMonth

class ErrorHandlingTest extends AnyFlatSpec with Matchers {
  "StudentValidator" should "handle valid and invalid students" in {
    import ValidationError._

    CreditCardValidator.validate(
      name = "Genrihs",
      number = "1234567890123456",
      expirationDate = "12/20",
      securityCode = "123"
    ) shouldBe CreditCard("Genrihs", "1234567890123456", YearMonth.parse("2020-12"), "123").validNec

    CreditCardValidator.validate(
      name = "Genrihs Saruhanovs",
      number = "1234567890123456",
      expirationDate = "12/20",
      securityCode = "123"
    ) shouldBe CreditCard(
      "Genrihs Saruhanovs",
      "1234567890123456",
      YearMonth.parse("2020-12"),
      "123"
    ).validNec

    CreditCardValidator.validate(
      name = "Genrihs",
      number = "1234567890123456",
      expirationDate = "12/20",
      securityCode = "123"
    ) shouldBe CreditCard("Genrihs", "1234567890123456", YearMonth.parse("2020-12"), "123").validNec

    def checkInvalid(
      name: String,
      number: String,
      expirationDate: String,
      securityCode: String,
      errors: Set[ValidationError]
    ): Assertion =
      CreditCardValidator
        .validate(
          name = name,
          number = number,
          expirationDate = expirationDate,
          securityCode = securityCode
        )
        .leftMap(_.toList.toSet) shouldBe errors.invalid
//
    checkInvalid(
      name = "Genrihs111",
      number = "1234567890123456",
      expirationDate = "12/20",
      securityCode = "123",
      errors = Set(NameHasSpecialCharacters),
    )
    checkInvalid(
      name = "Genrihs111",
      number = "12345678901234561",
      expirationDate = "12/20",
      securityCode = "123",
      errors = Set(NameHasSpecialCharacters, CardNumberLengthMismatch),
    )
    checkInvalid(
      name = "Genrihs111",
      number = "12345678901234561",
      expirationDate = "12/20",
      securityCode = "1AZ",
      errors = Set(NameHasSpecialCharacters, CardNumberLengthMismatch, SecurityCodeIsNotNumeric),
    )

    checkInvalid(
      name = "Genrihs111",
      number = "12345678901234561",
      expirationDate = "12/20",
      securityCode = "1AZ1",
      errors = Set(
        NameHasSpecialCharacters,
        CardNumberLengthMismatch,
        SecurityCodeLengthMismatch,
        SecurityCodeIsNotNumeric
      ),
    )

    checkInvalid(
      name = "Genrihs111",
      number = "12345678901234561L",
      expirationDate = "12/20",
      securityCode = "1AZ1",
      errors = Set(
        NameHasSpecialCharacters,
        CardNumberIsNotNumeric,
        CardNumberLengthMismatch,
        SecurityCodeLengthMismatch,
        SecurityCodeIsNotNumeric
      ),
    )

    checkInvalid(
      name = "Genrihs",
      number = "1234567890123456",
      expirationDate = "1220",
      securityCode = "123",
      errors = Set(DateFormatMismatch),
    )

    checkInvalid(
      name = "Genrihs",
      number = "1234567890123456",
      expirationDate = "12/19",
      securityCode = "123",
      errors = Set(CreditCardIsExpired),
    )
  }
}

package evo.homework

import evo.homework.EffectsHomeworkDeclarative._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Try

class EffectsHomeworkDeclarativeSpec extends AnyFreeSpec with Matchers {
  "Effects homework IO functionality" - {
    "should work with different constructs and conversions" - {

      "via apply" - {
        "should return a value when unsafe run is correct" in {
          val testIo = IO("1".toInt)
          testIo.unsafeRunSync() shouldEqual 1
        }

        "should throw an error when unsafe run is not correct" in {
          val testIo = IO("test".toInt)
          an[NumberFormatException] should be thrownBy testIo.unsafeRunSync()
        }

      }
      "via Either" - {
        "should return a value when unsafe run is correct" in {
          val testIo = IO.fromEither(Try("1".toInt).toEither)
          testIo.unsafeRunSync() shouldEqual 1
        }

        "should throw an error when unsafe run is not correct" in {
          val testIo = IO.fromEither(Try("test".toInt).toEither)
          an[NumberFormatException] should be thrownBy testIo.unsafeRunSync()
        }
      }

      "via Option" - {
        "should return a value when unsafe run is correct" in {
          val testIo = IO.fromOption("1".toIntOption)(orElse = new NumberFormatException)
          testIo.unsafeRunSync() shouldEqual 1
        }

        "should throw an error when unsafe run is not correct" in {
          val testIo = IO.fromOption("test".toIntOption)(orElse = new NumberFormatException)
          an[NumberFormatException] should be thrownBy testIo.unsafeRunSync()
        }
      }

      "via Try" - {
        "should return a value when unsafe run is correct" in {
          val testIo = IO.fromTry(Try("1".toInt))
          testIo.unsafeRunSync() shouldEqual 1
        }

        "should throw an error when unsafe run is not correct" in {
          val testIo = IO.fromTry(Try("test".toInt))
          an[NumberFormatException] should be thrownBy testIo.unsafeRunSync()
        }
      }
      "via multiple conversions" - {
        "should handle success run when executing attempt" in {
          val testIo = IO("1".toInt)
          testIo.attempt.unsafeRunSync() shouldEqual Right(1)
        }

        "*> should return the last execution result if all are successful" in {
          val testIo = IO("5".toInt) *> IO("1".toInt) *> IO("10".toInt)
          testIo.unsafeRunSync() shouldEqual 10
        }

        "*> should return the error if any execution was faulty" in {
          val testIo = IO("5".toInt) *> IO("test".toInt) *> IO("10".toInt)
          an[NumberFormatException] should be thrownBy testIo.unsafeRunSync()
        }

        "void should suppress the result" in {
          val testIo = IO("5".toInt).void
          testIo.unsafeRunSync() shouldEqual ()
        }

        "as should overwrite the result" in {
          val testIo = IO("5".toInt).as("belissimo")
          testIo.unsafeRunSync() shouldEqual "belissimo"
        }

        "should return option when requested" in {
          val testIo = IO("5".toInt).option
          testIo.unsafeRunSync() shouldEqual Some(5)
        }

        "should redeem success" in {
          val testIo = IO("5".toInt)
          testIo.redeem(_ => "error", _.toString).unsafeRunSync() shouldEqual "5"
        }

        "should redeem failure" in {
          val testIo = IO("test".toInt)
          testIo.redeem(_ => "error", _.toString).unsafeRunSync() shouldEqual "error"
        }

//        "suspend should suspend value" in {
//          val testIo = IO(IO("5".toInt))
//          IO.suspend(testIo).unsafeRunSync() shouldEqual IO("5".toInt).unsafeRunSync()
//        }
      }
    }

    "should work with map and flatmap" - {

      "should map the success result" in {
        val testIo = IO("1".toInt).map(_.toString)
        testIo.unsafeRunSync() shouldEqual "1"
      }

      "map should return an error if failure" in {
        val testIo = IO("test".toInt).map(_.toString)
        an[NumberFormatException] should be thrownBy testIo.unsafeRunSync()
      }

      "should flatmap the success result" in {
        val testIo = IO("1".toInt).flatMap(x => IO(x + 1))
        testIo.unsafeRunSync() shouldEqual 2
      }

      "should flatmap the result as error if failure" in {
        val testIo = IO("test".toInt).flatMap(x => IO(x + 1))
        an[NumberFormatException] should be thrownBy testIo.unsafeRunSync()
        //      testIo.unsafeRunSync() shouldEqual 2
      }

      "should work with for comprehension when success values" in {
        val testIo = for {
          x1 <- IO("1".toInt)
          x2 <- IO(1)
        } yield x1 + x2
        testIo.unsafeRunSync() shouldEqual 2
      }

      "should work with for comprehension failure value" in {
        val testIo = for {
          x1 <- IO("test".toInt)
          x2 <- IO(1)
        } yield x1 + x2
        an[NumberFormatException] should be thrownBy testIo.unsafeRunSync()
      }
    }
  }
}

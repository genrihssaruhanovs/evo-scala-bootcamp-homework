package evo.homework

import scala.io.Source
import scala.util.{Failure, Success, Try}

object ControlStructures {

  sealed trait Command {
    def compute: Either[ErrorMessage, Double]
  }

  object Command {
    def validateCommand(x: String): Either[ErrorMessage, Unit] = {
      val commands = Set("divide", "sum", "average", "min", "max")
      if (commands.contains(x)) Right() else Left(ErrorMessage("Wrong command"))
    }

    def getCommand(x: String,
                   numbers: List[Double]): Either[ErrorMessage, Command] = {
      x match {
        case "divide" =>
          if (numbers.size == 2) Right(Divide(numbers.head, numbers.last))
          else
            Left(
              ErrorMessage("Divide command must only have dividend and divisor")
            )
        case "sum" => Right(Sum(numbers))
        case "min" => Right(Min(numbers))
        case "max" => Right(Max(numbers))
        case "average" => Right(Average(numbers))
      }
    }

    final case class Divide(dividend: Double, divisor: Double) extends Command {
      override def compute: Either[ErrorMessage, Double] =
        if (divisor == 0) Left(ErrorMessage("Cannot divide by 0"))
        else Right(dividend / divisor)
    }

    final case class Sum(numbers: List[Double]) extends Command {
      override def compute: Either[ErrorMessage, Double] =
        Right(numbers.sum)
    }

    final case class Average(numbers: List[Double]) extends Command {
      override def compute: Either[ErrorMessage, Double] =
        Right(numbers.sum / numbers.size)
    }

    final case class Min(numbers: List[Double]) extends Command {
      override def compute: Either[ErrorMessage, Double] =
        Right(numbers.min)
    }

    final case class Max(numbers: List[Double]) extends Command {
      override def compute: Either[ErrorMessage, Double] =
        Right(numbers.max)
    }

  }

  final case class ErrorMessage(value: String)

  sealed trait Result {
    def command: Command

    def result: Double
  }

  final case class CalcResult(command: Command, result: Double) extends Result

  def parseCommand(x: String): Either[ErrorMessage, Command] = {

    val convertList: List[String] => Either[ErrorMessage, List[Double]] = {
      case Nil => Left(ErrorMessage("No parameters for calculation"))
      case list =>
        Try(list.filter(_ != "").map(_.toDouble)) match {
          case Failure(_) => Left(ErrorMessage("Inconsistent parameters"))
          case Success(v) => Right(v)
        }
    }

    x.split(" ").toList match {
      case Nil => Left(ErrorMessage("Empty input"))
      case x :: xs =>
        for {
          _ <- Command.validateCommand(x)
          parsedList <- convertList(xs)
          command <- Command.getCommand(x, parsedList)
        } yield command
    }
  }

  def calculate(command: Command): Either[ErrorMessage, Result] = {
    command.compute.fold(error => Left(error), result => Right(CalcResult(command, result)))
  }

  def renderResult(x: Result): String = {
    import java.text.DecimalFormat
    val format = new DecimalFormat("#.###")

    import evo.homework.ControlStructures.Command._
    x.command match {
      case Divide(dividend, divisor) =>
        s"${format.format(dividend)} divided by ${format.format(divisor)} is ${format.format(x.result)}"
      case Sum(numbers) =>
        s"the sum of ${numbers.map(format.format).mkString(" ")} is ${format.format(x.result)}"
      case Average(numbers) =>
        s"the average of ${numbers.map(format.format).mkString(" ")} is ${format.format(x.result)}"
      case Min(numbers) =>
        s"the minimum of ${numbers.map(format.format).mkString(" ")} is ${format.format(x.result)}"
      case Max(numbers) =>
        s"the maximum of ${numbers.map(format.format).mkString(" ")} is ${format.format(x.result)}"
    }
  }

  def process(x: String): String = {
    val result = for {
      command <- parseCommand(x)
      result <- calculate(command)
    } yield renderResult(result)

    result.fold(l => s"Error: ${l.value}", r => r)
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit =
    Source.stdin.getLines() map process foreach println
}

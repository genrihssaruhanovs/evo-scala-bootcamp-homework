package evo.homework

import java.util.concurrent.Executors

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/*
 * Homework 1. Provide your own implementation of a subset of `IO` functionality.
 *
 * Provide also tests for this functionality in EffectsHomework1Spec (which you should create).
 *
 * Refer to:
 *  - https://typelevel.org/cats-effect/datatypes/io.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO$.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO.html
 * about the meaning of each method as needed.
 *
 * There are two main ways how to implement IO:
 * - Executable encoding  - express every constructor and operator for our model in terms of its execution
 * - Declarative encoding - express every constructor and operator for our model as pure data in a recursive
 *                          tree structure
 *
 * While the real Cats Effect IO implementation uses declarative encoding, it will be easier to solve this
 * task using executable encoding, that is:
 *  - Add a `private val run: () => A` parameter to the class `IO` private constructor
 *  - Have most of the methods return a `new IO(...)`
 *
 * Ask questions in the bootcamp chat if stuck on this task.
 */
object EffectsHomeworkDeclarative {
  private implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  sealed trait IO[A] {
    def map[B](f: A => B): IO[B] = Map(this, f)
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)

    def *>[B](another: IO[B]): IO[B] = flatMap(_ => another)

    def as[B](newValue: => B): IO[B] = map(_ => newValue)

    def void: IO[Unit] = map(_ => ())
    def attempt: IO[Either[Throwable, A]] = IO(Try(unwrap(this)).toEither)
    def option: IO[Option[A]] = IO(Try(unwrap(this)).toOption)

    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] =
      IO.suspend(Try(unwrap(this)) match {
        case Success(value)     => IO(value)
        case Failure(exception) => f(exception)
      })

    def redeem[B](recover: Throwable => B, map: A => B): IO[B] =
      IO.suspend(Try(unwrap(this)) match {
        case Success(value)     => IO(map(value))
        case Failure(exception) => IO(recover(exception))
      })

    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] =
      IO.suspend(Try(unwrap(this)) match {
        case Success(value)     => bind(value)
        case Failure(exception) => recover(exception)
      })

    def unsafeRunSync(): A = unwrap(this)

    def unsafeToFuture(): Future[A] = Future { unsafeRunSync() }

    @tailrec
    def unwrap(io: IO[A]) : A = {
      io match {
        case Delay(thunk) => thunk()
        case Suspend(thunk) => thunk().unsafeRunSync()
        case Pure(a) => a
        case RaiseError(e) => throw e
        case Map(source, f) => f(source.unsafeRunSync())
        case FlatMap(source, f) => unwrap(f(source.unsafeRunSync()))
      }
    }
  }

  final case class Delay[A](thunk: () => A) extends IO[A]
  final case class Pure[A](value: A) extends IO[A]
  final case class RaiseError[A](e: Throwable) extends IO[A]
  final case class Suspend[A](thunk: () => IO[A]) extends IO[A]
  final case class Map[A, B](source: IO[A], f: A => B) extends IO[B]
  final case class FlatMap[A, B](source: IO[A], f: A => IO[B]) extends IO[B]

  object IO {
    def apply[A](body: => A): IO[A] = delay(body)
    def suspend[A](thunk: => IO[A]): IO[A] = Suspend(() => thunk)
    def delay[A](body: => A): IO[A] = Delay(() => body)
    def pure[A](a: A): IO[A] = Pure(a)
    def fromEither[A](e: Either[Throwable, A]): IO[A] = e match {
      case Right(value) => IO(value)
      case Left(exception) => RaiseError(exception)
    }
    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = option match {
      case Some(value) => IO(value)
      case None => RaiseError(orElse)
    }
    def fromTry[A](t: Try[A]): IO[A] = t match {
      case Success(value) => IO(value)
      case Failure(exception) => RaiseError(exception)
    }
    def none[A]: IO[Option[A]] = IO(None)

    def raiseError[A](e: Throwable): IO[A] = RaiseError(e)

    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] =
      if (!cond) raiseError(e) else unit

    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] =
      if (cond) raiseError(e) else unit

    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] =
      if (!cond) action else unit

    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] =
      if (cond) action else unit

    val unit: IO[Unit] = IO(())
  }
}

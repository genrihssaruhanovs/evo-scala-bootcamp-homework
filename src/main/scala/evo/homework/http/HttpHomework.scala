package evo.homework.http

import java.util.InputMismatchException
import java.util.concurrent.atomic.AtomicReference

import cats.effect.concurrent.Ref
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxFlatMapOps
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s._
import org.http4s.client.Client
import org.http4s.headers._
import org.http4s.util.CaseInsensitiveString

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt
import scala.io.StdIn
import scala.util.Random

// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed, as well as the maximum number of attempts.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// Use HTTP or WebSocket for communication. The exact protocol and message format to use is not specified and
// should be designed while working on the task.

import java.util.UUID.randomUUID

object GuessServer extends IOApp {

  val sessionsRef: IO[Ref[IO, Map[String, Session]]] =
    Ref.of[IO, Map[String, Session]](Map.empty[String, Session])
  case class Session(id: String, numberToGuess: Int, attemptsLeft: Int)

  var sessions: mutable.Map[String, Session] =
    scala.collection.mutable.Map[String, Session]()

//  val sessionCache: IO[SessionCache] = for {
//    ref <- Ref[IO].of(Map.empty[String, Session])
//  } yield new SessionCache(ref)

  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)

  private val guessRoute = HttpRoutes.of[IO] {
    case req @ GET -> Root / "start" / IntVar(min) / IntVar(max) / IntVar(
          attempts
        ) =>
      val numberToGuess = new Random().nextInt(max + 1 - min) + min
      val sessionId     = randomUUID().toString
      val session       = Session(sessionId, numberToGuess, attempts)

//      for {
//        sessions <- sessionsRef
//        _        <- sessions.update(sessions => sessions + (session.id -> session))
//      } yield ()

      sessions += (session.id -> session)
      Ok("Game is created").map(
        _.addCookie(ResponseCookie("sessionId", session.id))
      )
    //      val requestHeaders = req.headers
    //      val result = for {
    //        minHeader <- requestHeaders.find(_.name == "min")
    //        min <- minHeader.value.toIntOption
    //        maxHeader <- requestHeaders.find(_.name == "max")
    //        max <- maxHeader.value.toIntOption
    //        attemptsHeader <- requestHeaders.find(_.name == "attempts")
    //        attempts <- attemptsHeader.value.toIntOption
    //        numberToGuess = new Random().nextInt(max + 1 - min) + min
    //        sessionId = randomUUID().toString
    //      } yield Session(sessionId, numberToGuess, 0, attempts)
    //
    //      result match {
    //        case Some(session) =>
    //          sessions += (session.id -> session)
    //          Ok("Game is created").map(_.addCookie(ResponseCookie("sessionId", session.id)))
    //        case None => BadRequest("Incorrect data passed")
    //      }

    case req @ GET -> Root / "guess" / number =>
      val session = for {
        sessionIdCookie <- req.cookies.find(_.name == "sessionId")
//        sessions        <- sessionsRef.unsafeRunSync()
        session <- sessions.get(sessionIdCookie.content)
      } yield session

      session match {
        case Some(session) =>
          val attemptsLeft = session.attemptsLeft - 1
          number.toIntOption match {
            case Some(value) =>
              val result = Integer.compare(value, session.numberToGuess)
              val string = result match {
                case -1 =>
                  s"Number to guess is higher. You have $attemptsLeft attempts left"
                case 1 =>
                  s"Number to guess is lower. You have $attemptsLeft attempts left"
                case 0 => "You guessed the number!"
              }
              if (result != 0 && attemptsLeft == 0) {
                sessions -= session.id
                Ok(
                  s"You ran out of attempts! The number was ${session.numberToGuess}",
                  Header("GameOver", true.toString)
                )
              } else if (result == 0) {
                sessions -= session.id
                Ok(string, Header("GameOver", true.toString))
              } else {
                sessions(session.id) = Session(
                  session.id,
                  session.numberToGuess,
                  attemptsLeft
                )
                Ok(string, Header("GameOver", false.toString))
              }
            case None => BadRequest("Incorrect number entry")
          }
        case None => BadRequest("Game has not started yet")
      }
  }

  private[http] val httpApp = {
    guessRoute
  }.orNotFound
}

object GuessClient extends IOApp {
  private val uri = uri"http://localhost:9001"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  private def readLine: IO[String] = IO(StdIn.readLine())

  def promptStartParameters(): IO[(String, String, String)] = {
    for {
      min      <- printLine("Enter minimal bound") *> readLine
      max      <- printLine("Enter maximal bound") *> readLine
      attempts <- printLine("Enter attempts count") *> readLine
    } yield (min, max, attempts)
  }

  def guessNumber(
      client: Client[IO],
      sessionCookie: ResponseCookie
  ): IO[Unit] = {
    for {
      numberIn <- printLine(s"Guess the number") *> readLine
      guessAttemptRequest <-
        Method
          .GET(uri / "guess" / numberIn)
          .map(
            _.addCookie(
              RequestCookie(sessionCookie.name, sessionCookie.content)
            )
          )
      response <-
        client.run(guessAttemptRequest).use { response => IO(response) }
      _ <- response.as[String] >>= printLine
      isGameOver = response.headers.find(
        _.name == CaseInsensitiveString("GameOver")
      ) match {
        case Some(v) => v.value.toBoolean
        case None    => false
      }
      _ <- if (isGameOver) IO.unit else guessNumber(client, sessionCookie)
    } yield ()
  }

  def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](ExecutionContext.global).resource
      .parZip(Blocker[IO])
      .use {
        case (client, blocker) =>
          for {
            _ <- printLine(
              "Guessing game has started. Enter minimal and maximal bounds, attempt count. Then try to guess the number"
            )
            entry <- promptStartParameters()
            (min, max, attempts) = entry
            startGameRequest <- Method.GET(uri / "start" / min / max / attempts)
            responseCookies <- client.run(startGameRequest).use { response =>
              IO(response.cookies)
            }
            sessionCookie <- IO.fromOption(
              responseCookies.find(_.name == "sessionId")
            )(throw new InputMismatchException)
            _ <- guessNumber(client, sessionCookie)
            _ <- printLine("Game over")
          } yield ()
      }
      .as(ExitCode.Success)
}

object Test extends IOApp {
  private val uri = uri"http://localhost:9001"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  //  private def readLine: IO[String] = IO(StdIn.readLine())

  def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- GuessServer.run(List.empty).start
      _ <- IO.sleep(1.second)
      _ <- GuessClient.run(List.empty)
    } yield ExitCode.Success
  }
}

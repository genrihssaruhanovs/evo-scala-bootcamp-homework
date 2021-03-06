package evo.homework.http

import java.util.InputMismatchException
import java.util.concurrent.atomic.AtomicReference

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxFlatMapOps
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s._
import org.http4s.client.Client
import org.http4s.util.CaseInsensitiveString

import scala.concurrent.ExecutionContext
import scala.io.StdIn
import scala.util.Random
import java.util.UUID.randomUUID

import cats.effect.concurrent.Ref

object GuessServer extends IOApp {
  case class Session(id: String, numberToGuess: Int, attemptsLeft: Int)

//    val test: IO[Ref[IO, Map[String, Session]]] =
//      Ref.of[IO, Map[String, Session]](Map.empty[String, Session])
  private val sessionsRef: AtomicReference[Map[String, Session]] =
    new AtomicReference(Map.empty[String, Session])

  override def run(args: List[String]): IO[ExitCode] = for {
    sessionsRef2 <- Ref.of[IO, Map[String, Session]](Map.empty)
    _ <- BlazeServerBuilder[IO] (ExecutionContext.global)
    .bindHttp(port = 9001, host = "localhost")
    .withHttpApp(httpApp)
    .serve
     .compile
     .drain
//     .as(ExitCode.Success)
  } yield ExitCode.Success
  private val guessRoute = HttpRoutes.of[IO] {
    case GET -> Root / "start" / IntVar(min) / IntVar(max) / IntVar(
          attempts
        ) =>
      val numberToGuess = new Random().nextInt(max + 1 - min) + min
      val sessionId     = randomUUID().toString
      val session       = Session(sessionId, numberToGuess, attempts)

//      for {
//        sessions <- sessionsRef
//        _        <- sessions.update(sessions => sessions + (session.id -> session))
//      } yield ()
      sessionsRef.updateAndGet(sessions => sessions + (session.id -> session))

      Ok("Game is created").map(
        _.addCookie(ResponseCookie("sessionId", session.id))
      )

    case req @ GET -> Root / "guess" / number =>
      val session = for {
        sessionIdCookie <- req.cookies.find(_.name == "sessionId")
        sessions = sessionsRef.get()
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
                sessionsRef.updateAndGet(sessions => sessions - session.id)
                Ok(
                  s"You ran out of attempts! The number was ${session.numberToGuess}",
                  Header("GameOver", true.toString)
                )
              } else if (result == 0) {
                sessionsRef.updateAndGet(sessions => sessions - session.id)
                Ok(string, Header("GameOver", true.toString))
              } else {
                sessionsRef.updateAndGet(sessions =>
                  sessions + (session.id -> Session(
                    session.id,
                    session.numberToGuess,
                    attemptsLeft
                  ))
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

  // to be reworked in a recursion until user entery is all numeric
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
      .use { client =>
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
          )(throw new InputMismatchException) //to be reworked
          _ <- guessNumber(client, sessionCookie)
          _ <- printLine("Game over")
        } yield ()
      }
      .as(ExitCode.Success)
}

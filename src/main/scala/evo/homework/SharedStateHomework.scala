package evo.homework

import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.{Clock, Concurrent, ExitCode, IO, IOApp, Timer}
import cats.implicits._

import scala.concurrent.duration._

/*
 * Please implement a Cache which allows concurrent access.
 *
 * Tip: checking expiration could be represented as some infinite process somewhere in background
 *
 * Tip: you can use following structure to get current time suspended in effect : Clock[F].realTime(MILLISECONDS).flatMap(...)
 *
 * Cached items should have an expiration timestamp after which they are evicted.
 *
 * If we will put a value with the same key then it should renew expiration
 */
object SharedStateHomework extends IOApp {

  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]

    def put(key: K, value: V): F[Unit]
  }

  class RefCache[F[_]: Clock: Monad, K, V](state: Ref[F, Map[K, (Long, V)]])
    extends Cache[F, K, V] {

    def get(key: K): F[Option[V]] = {
      state.get
        .map(
          _.get(key)
            .map { case (_, value) => value }
        )
    }

    def put(key: K, value: V): F[Unit] =
      for {
        s <- state.get
        currentTime <- Clock[F].realTime(MILLISECONDS)
        _ <- state.update(_ => s + (key -> (currentTime, value)))
      } yield ()

  }

  object Cache {

    def of[F[_]: Clock, K, V](
                               expiresIn: FiniteDuration,
                               checkOnExpirationEvery: FiniteDuration
                             )(implicit T: Timer[F], C: Concurrent[F]): F[Cache[F, K, V]] = {

      def deleteExpired(state: Ref[F, Map[K, (Long, V)]]): F[Unit] = {
        for {
          _ <- T.sleep(checkOnExpirationEvery)
          map <- state.get
          currentTime <- Clock[F].realTime(MILLISECONDS)
          filteredMap = map.filter {
            case (_, (timestamp, _)) =>
              timestamp > currentTime - expiresIn.toMillis
          }
          _ <- state.update(_ => filteredMap)
        } yield ()
      }

      for {
        state <- Ref[F].of(Map.empty[K, (Long, V)])
        _ <- C.start(deleteExpired(state).foreverM.void)
      } yield new RefCache(state)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {

    for {
      cache <- Cache.of[IO, Int, String](10.seconds, 4.seconds)
      _ <- cache.put(1, "Hello")
      _ <- cache.put(2, "World")
      _ <- cache
        .get(1)
        .flatMap(
          s =>
            IO {
              println(s"first key $s")
            }
        )
      _ <- cache
        .get(2)
        .flatMap(
          s =>
            IO {
              println(s"second key $s")
            }
        )
      _ <- IO.sleep(12.seconds)
      _ <- cache
        .get(1)
        .flatMap(
          s =>
            IO {
              println(s"first key $s")
            }
        )
      _ <- cache
        .get(2)
        .flatMap(
          s =>
            IO {
              println(s"second key $s")
            }
        )
      _ <- IO.sleep(12.seconds)
      _ <- cache
        .get(1)
        .flatMap(
          s =>
            IO {
              println(s"first key $s")
            }
        )
      _ <- cache
        .get(2)
        .flatMap(
          s =>
            IO {
              println(s"second key $s")
            }
        )
    } yield ExitCode.Success
  }
}

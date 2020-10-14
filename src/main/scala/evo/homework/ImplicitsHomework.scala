package evo.homework

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object ImplicitsHomework {

  /**
   * Lo and behold! Brand new super-useful collection library for Scala!
   *
   * Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]],
   * a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size
   * of the data stored.
   *
   * As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use
   * a thing called size score. Its calculation rules:
   * - size score of a Byte is 1
   * - Int - 4 (as primitive JVM int consists of 4 bytes)
   * - Long - 8
   * - Char - 2 (one UTF-16 symbol is 2 bytes)
   * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
   * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
   * the fields
   * - score for any sequence (Array[T], List[T], Vector[T]) is
   * 12 (our old friend object header) + sum of scores of all elements
   * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
   */
  object SuperVipCollections4s {
    type SizeScore = Int

    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object syntax {

      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
        def sizeScore: SizeScore = implicitly[GetSizeScore[T]].apply(inner)
      }

    }

    /**
     * Mutable key-value cache which limits the size score of the data scored.
     *
     * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
     * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
     * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
     * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
     *
     * @param maxSizeScore max size score for the stored data
     * @tparam K key type
     * @tparam V value type
     */
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {
      //with this you can use .sizeScore syntax on keys and values

      import syntax._

      /*
      mutable.LinkedHashMap is a mutable map container which preserves insertion order - this might be useful!
       */
      private val map = mutable.LinkedHashMap.empty[K, V]

      def getCurrentMapSize: Int = map.map { case (key, value) => key.sizeScore + value.sizeScore }.sum

      @tailrec
      def put(key: K, value: V): Unit = {
        if (getCurrentMapSize + key.sizeScore + value.sizeScore <= maxSizeScore) {
          map += (key -> value)
        } else {
          if (map.nonEmpty) {
            val(headKey, _) = map.head
            map -= headKey
            //can you make 2 lines above in one line?
            put(key, value)
          }
        }
      }

      def get(key: K): Option[V] = map.get(key)
    }

    /**
     * Cool custom immutable multi-map collection - does not extend the standard library collection types
     * (yes, this is a feature)
     */
    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])

    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()

      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
    }

    /**
     * Type-class allowing us to iterate over different "collection-like" types with one type arg
     */
    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }

    /**
     * Same as [[Iterate]] but for collections containing 2 types of values (think Map's and like)
     */
    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]

      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object instances {

      import syntax._

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }
      //Array is not an Iterable in Scala 2.13 but we still might abstract over iteration logic for both!
      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }

      //      Attamt to make generic 2 parameter collection iterator, something is missing

      //      implicit val mapIterate: Iterate2[Iterable[(_, _)]] = new Iterate2[Iterable[(_, _)]] {
      //        override def iterator1[T, S](f: Iterable[(T, S)]): Iterator[T] = f.map { case (key, _) => key }.iterator
      //        override def iterator2[T, S](f: Iterable[(T, S)]): Iterator[S] = f.map { case (_, value) => value }.iterator
      //      }
      implicit val mapIterate: Iterate2[Map] = new Iterate2[Map] {
        override def iterator1[T, S](f: Map[T, S]): Iterator[T] = f.keys.iterator

        override def iterator2[T, S](f: Map[T, S]): Iterator[S] = f.values.iterator
      }

      implicit val multiMapIterate: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
        override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] = f.inner.map { case (key, _) => key }.iterator

        override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] = f.inner.map { case (_, value) => value }.iterator
      }

      /*
      replace this big guy with proper implicit instances for types:
      - Byte, Char, Int, Long
      - String
      - Array[T], List[T], Vector[T], Map[K,V], PackedMultiMap[K,V]
        - points to karma if you provide those in a generic way
        (Iterate and Iterate2 type-classes might be helpful!)
      */

      implicit def byteGetSizeScore: GetSizeScore[Byte] = (_: Byte) => 1

      implicit def intGetSizeScore: GetSizeScore[Int] = (_: Int) => 4

      implicit def longGetSizeScore: GetSizeScore[Long] = (_: Long) => 8

      implicit def charGetSizeScore: GetSizeScore[Char] = (_: Char) => 2

      implicit def stringGetSizeScore: GetSizeScore[String] = (string: String) => 12 + string.length * 2


      //      Attempts to make List, Vector etc. generic using Iterable type-class
      //      implicit def iterableGetSizeScore[A: GetSizeScore]: GetSizeScore[Iterable[A]] = (iterate: Iterable[A]) => iterate.foldLeft(12)((x, y) => x + y.sizeScore)
      //      implicit def iterableGetSizeScore[A: GetSizeScore]: GetSizeScore[Iterable[A]: Iterate] = (iterate: Iterable[A]) => ???
      //      implicit def iterableGetSizeScore[A: GetSizeScore, T[A]: Iterate]: GetSizeScore[T[A]] = (iterate: T[A]) => value.iterator  <-- seem to be the closest one, but couldn't get values from iterate


      implicit def arrayGetSizeScore[A: GetSizeScore]: GetSizeScore[Array[A]] = (array: Array[A]) =>
        array.foldLeft(12)((x, y) => x + y.sizeScore)

      implicit def listGetSizeScore[A: GetSizeScore]: GetSizeScore[List[A]] = (list: List[A]) =>
        list.foldLeft(12)((x, y) => x + y.sizeScore)

      implicit def vectorGetSizeScore[A: GetSizeScore]: GetSizeScore[Vector[A]] = (vector: Vector[A]) =>
        vector.foldLeft(12)((x, y) => x + y.sizeScore)

      implicit def mapGetSizeScore[A: GetSizeScore, B: GetSizeScore]: GetSizeScore[Map[A, B]] = (map: Map[A, B]) =>
        12 + map.map { case (key, value) => key.sizeScore + value.sizeScore }.sum

      implicit def pMapGetSizeScore[A: GetSizeScore, B: GetSizeScore]: GetSizeScore[PackedMultiMap[A, B]] = (pMap: PackedMultiMap[A, B]) =>
        12 + pMap.inner.map { case (key, value) => key.sizeScore + value.sizeScore }.sum

    }

  }

  /*
  Time to bring some business value!
  #GoodVibes #ThrowbackThursday #NoFilter #squadgoals
   */
  object MyTwitter {

    import SuperVipCollections4s._
    import instances._
    import syntax._

    final case class Twit(
                           id: Long,
                           userId: Int,
                           hashTags: Vector[String],
                           attributes: PackedMultiMap[String, String],
                           fbiNotes: List[FbiNote],
                         )

    final case class FbiNote(
                              month: String,
                              favouriteChar: Char,
                              watchedPewDiePieTimes: Long,
                            )

    trait TwitCache {
      def put(twit: Twit): Unit

      def get(id: Long): Option[Twit]
    }

    implicit def fbiGetSizeScore: GetSizeScore[FbiNote] = (note: FbiNote) =>
      note.month.sizeScore +
        note.favouriteChar.sizeScore +
        note.watchedPewDiePieTimes.sizeScore

    implicit def twitGetSizeScore: GetSizeScore[Twit] = (twit: Twit) => {
      twit.id.sizeScore +
        twit.userId.sizeScore +
        twit.hashTags.sizeScore +
        twit.attributes.sizeScore +
        twit.fbiNotes.sizeScore
    }

    /*
    Return an implementation based on MutableBoundedCache[Long, Twit]
     */
    def createTwitCache(maxSizeScore: SizeScore): TwitCache = new TwitCache {
      val mutableCache = new MutableBoundedCache[Long, Twit](maxSizeScore)

      override def put(twit: Twit): Unit = mutableCache.put(twit.id, twit)

      override def get(id: Long): Option[Twit] = mutableCache.get(id)
    }
  }

}

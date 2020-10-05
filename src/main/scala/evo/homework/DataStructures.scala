package evo.homework

object DataStructures {
  def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] = {

    val parsedMap: Map[Set[T], Int] = for {
      (value, groupedCharValues) <- map.groupBy { case (_, v) => v }
      parsedCharSet = groupedCharValues.foldLeft(Set[T]())((resultSet, charValue) => {
        charValue match {
          case (character, _) => resultSet + character
        }
      })
    } yield (parsedCharSet, value)

    parsedMap.toSeq.sortBy { case (_, v) => v }.toList
  }
}

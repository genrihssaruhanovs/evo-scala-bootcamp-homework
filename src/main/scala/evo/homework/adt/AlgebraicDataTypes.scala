package evo.homework.adt

object AlgebraicDataTypes {

  case class ErrorMessage(message: String)

  sealed trait Suit

  object Suit {
    final case object Heart extends Suit
    final case object Spade extends Suit
    final case object Diamond extends Suit
    final case object Club extends Suit

    def getFromChar(charValue: Char): Either[ErrorMessage, Suit] = {
      charValue match {
        case 'h' => Right(Heart)
        case 's' => Right(Spade)
        case 'd' => Right(Diamond)
        case 'c' => Right(Club)
        case other =>
          Left(ErrorMessage(s"$other is an incorrect value of suit"))
      }
    }
  }

  sealed abstract class Rank(val strength: Int)
  object Rank {
    final case object Two extends Rank(2)
    final case object Three extends Rank(3)
    final case object Four extends Rank(4)
    final case object Five extends Rank(5)
    final case object Six extends Rank(6)
    final case object Seven extends Rank(7)
    final case object Eight extends Rank(8)
    final case object Nine extends Rank(9)
    final case object Ten extends Rank(10)
    final case object Jack extends Rank(11)
    final case object Queen extends Rank(12)
    final case object King extends Rank(13)
    final case object Ace extends Rank(14)

    def getFromChar(charValue: Char): Either[ErrorMessage, Rank] = {
      charValue match {
        case '2' => Right(Two)
        case '3' => Right(Three)
        case '4' => Right(Four)
        case '5' => Right(Five)
        case '6' => Right(Six)
        case '7' => Right(Seven)
        case '8' => Right(Eight)
        case '9' => Right(Nine)
        case 'T' => Right(Ten)
        case 'J' => Right(Jack)
        case 'Q' => Right(Queen)
        case 'K' => Right(King)
        case 'A' => Right(Ace)
        case other =>
          Left(ErrorMessage(s"$other is an incorrect value of rank"))
      }
    }
  }

  final case class Card private (rank: Rank, suit: Suit)
  object Card {
    def create(rank: Rank, suit: Suit): Card = Card(rank, suit)
    def create(input: String): Either[ErrorMessage, Card] = {
      input match {
        case null => Left(ErrorMessage("Empty input for card creation"))
        case input if input.length != 2 =>
          Left(ErrorMessage(s"$input is an incorrect input for card creation"))
        case input =>
          for {
            rank <- Rank.getFromChar(input(0))
            suit <- Suit.getFromChar(input(1))
          } yield Card(rank, suit)
      }
    }
  }

  sealed abstract class HoldemHandType(val handCardCount: Int)
  object HoldemHandType {
    final case object Texas extends HoldemHandType(2)
    final case object Omaha extends HoldemHandType(4)
  }

  sealed trait CardSet {
    val cards: List[Card]
  }

  object CardSet {
    def create(input: String,
               cardCount: Int): Either[ErrorMessage, List[Card]] = {
      input match {
        case null => Left(ErrorMessage("Empty input for card set"))
        case input if input.length != cardCount * 2 =>
          Left(ErrorMessage(s" $input cannot be parsed to $cardCount cards"))
        case input =>
          val cardCharValues = input.split("(?<=\\G.{2})").toList
          val cardList = cardCharValues.map(x => Card.create(x))
          val (errors, result) = cardList.partitionMap(identity)
          if (errors.isEmpty) Right(result)
          else Left(ErrorMessage(errors.mkString(" ")))
      }
    }

    def uniqueRanks(cards: List[Card]): Map[Rank, Int] = {
      val uniqueRankCounts = cards.groupBy(_.rank)
      uniqueRankCounts.map { case (rank, count) => (rank, count.size) }
    }

    def uniqueRankCount(cards: List[Card]): Set[Int] = {
      val uniqueRanksMap = uniqueRanks(cards)
      uniqueRanksMap.map { case (_, count) => count }.toSet
    }
  }

  final case class Board private (cards: List[Card]) extends CardSet
  object Board {
    def create(cards: List[Card]): Either[ErrorMessage, Board] = {
      if (cards.size == 5) Right(Board(cards))
      else Left(ErrorMessage("Card count mismatch when creating board"))
    }
    def create(input: String): Either[ErrorMessage, Board] = {
      CardSet.create(input, 5).fold(l => Left(l), r => Right(Board(r)))
    }
  }
//  sealed trait Hand[H <: Hand[H]]{
//    val cards: Set[Card]
//  }
//
//  final case class TexasHand private (cards: Set[Card]) extends Hand[TexasHand]
//  object TexasHand {
//    def create(cards: Set[Card]): Either[ErrorMessage, TexasHand] =
//      if (cards.size == HoldemHandType.Texas.handCardCount) Right(TexasHand(cards))
//          else Left(ErrorMessage("Card count mismatch when creating texas hand"))
//  }
//
//  final case class OmahaHand private (cards: Set[Card]) extends Hand[OmahaHand]
//  object TexasHand {
//    def create(cards: Set[Card]): Either[ErrorMessage, OmahaHand] =
//      if (cards.size == HoldemHandType.Texas.handCardCount) Right(OmahaHand(cards))
//      else Left(ErrorMessage("Card count mismatch when creating texas hand"))
//  }

  final case class Hand private (cards: List[Card]) extends CardSet
  object Hand {
    val handType
      : HoldemHandType = HoldemHandType.Omaha //To be redesigned, must be current holdem type

    def create(cards: List[Card]): Either[ErrorMessage, Hand] = {
      if (cards.size == handType.handCardCount) Right(Hand(cards))
      else Left(ErrorMessage("Card count mismatch when creating board"))
    }
    def create(input: String): Either[ErrorMessage, Hand] = {
      CardSet
        .create(input, handType.handCardCount)
        .fold(l => Left(l), r => Right(Hand(r)))
    }

    def getHands(input: List[String]): Either[ErrorMessage, List[Hand]] = {
      val (errors, hands) =
        input.map(x => Hand.create(x)).partitionMap(identity)
      if (errors.isEmpty) Right(hands)
      else Left(ErrorMessage(errors.mkString(" ")))
    }
  }

  final case class FiveCardSet private (cards: List[Card]) extends CardSet
  object FiveCardSet {
    def create(cards: List[Card]): Either[ErrorMessage, FiveCardSet] = {
      if (cards.size == 5) Right(FiveCardSet(cards))
      else
        Left(ErrorMessage("Card count mismatch when building evaluation set"))
    }
  }

  sealed abstract class Combination(val strength: Int) {
    def hasCombination(fiveCards: FiveCardSet): Boolean
  }

  object Combination {

    final case object HighCard extends Combination(1) {
      override def hasCombination(fiveCards: FiveCardSet): Boolean = true
    }

    final case object Pair extends Combination(2) {
      override def hasCombination(fiveCards: FiveCardSet): Boolean =
        CardSet.uniqueRankCount(fiveCards.cards).contains(2)
    }

    final case object TwoPairs extends Combination(3) {
      override def hasCombination(fiveCards: FiveCardSet): Boolean = {
        val uniqueRanks = CardSet.uniqueRanks(fiveCards.cards)
        val rankCounts = uniqueRanks.groupBy { case (_, count) => count }
        rankCounts.getOrElse(2, Map()).size == 2
      }
    }

    final case object ThreeOfAKind extends Combination(4) {
      override def hasCombination(fiveCards: FiveCardSet): Boolean =
        CardSet.uniqueRankCount(fiveCards.cards).contains(3)
    }

    final case object Straight extends Combination(5) {
      override def hasCombination(fiveCards: FiveCardSet): Boolean = {
        fiveCards.cards match {
          case x :: xs
              if x.rank.strength - xs.last.rank.strength == 4 || (x.rank == Rank.Ace && xs.head.rank == Rank.Four && xs.last.rank == Rank.Two) =>
            true
          case _ => false
        }
      }
    }

    final case object Flush extends Combination(6) {
      override def hasCombination(fiveCards: FiveCardSet): Boolean = {
        fiveCards.cards.map(_.suit).toSet.size == 1
      }
    }

    final case object FullHouse extends Combination(7) {
      override def hasCombination(fiveCards: FiveCardSet): Boolean = {
        val uniqueRankCounts = CardSet.uniqueRankCount(fiveCards.cards)
        uniqueRankCounts.contains(2) && uniqueRankCounts.contains(3)
      }
    }

    final case object FourOfAKind extends Combination(8) {
      override def hasCombination(fiveCards: FiveCardSet): Boolean =
        CardSet.uniqueRankCount(fiveCards.cards).contains(4)
    }

    final case object StraightFlush extends Combination(9) {
      override def hasCombination(fiveCards: FiveCardSet): Boolean =
        Straight.hasCombination(fiveCards) && Flush.hasCombination(fiveCards)
    }

    def getCombination(fiveCards: FiveCardSet): Combination = { // rework this method to return required combination
      if (StraightFlush.hasCombination(fiveCards)) StraightFlush
      else if (FourOfAKind.hasCombination(fiveCards)) FourOfAKind
      else if (FullHouse.hasCombination(fiveCards)) FullHouse
      else if (Flush.hasCombination(fiveCards)) Flush
      else if (Straight.hasCombination(fiveCards)) Straight
      else if (ThreeOfAKind.hasCombination(fiveCards)) ThreeOfAKind
      else if (TwoPairs.hasCombination(fiveCards)) TwoPairs
      else if (Pair.hasCombination(fiveCards)) Pair
      else HighCard
    }
  }

  final case class EvaluatedHand(hand: Map[Hand, Combination])
  object EvaluatedHand {
    def create(board: Board, hand: Hand): EvaluatedHand = {
      //val handAndBoard: List[Card] = board.cards :: hand.cards
      //val allFiveCardCombinations = handAndBoard.toSet.subsets(5).toList
      //val fiveCardSets = allFiveCardCombinations.map(x => FiveCardSet.create(x.toList))
      //TODO make above smooth, loop over fiveCardSets and get the best combination
      ???
    }
  }
  final case class EvaluatedGame(evaluatedHands: List[EvaluatedHand])

  final case class Game private (board: Board, hands: List[Hand])
  object Game {
    def create(input: String,
               omahaSwitch: Boolean): Either[ErrorMessage, Game] = {
      val gameMode = if (omahaSwitch) HoldemHandType.Omaha else HoldemHandType.Texas

      input.split(" ").toList match {
        case Nil => Left(ErrorMessage("Input string is blank"))
        case x :: xs =>
          for {
            board <- Board.create(x)
            hands <- Hand.getHands(xs)
          } yield Game(board, hands)
      }
    }

  }
}

package com.evolution.bootcamp.assignment.poker

import cats.implicits._
import com.evolution.bootcamp.assignment.poker.Rank._

sealed trait Value {
  def major: Int
  def descendingRanks: List[Rank]
}

object Value {
  implicit val ordering: Ordering[Value] = (x: Value, y: Value) => {
    val majorComparison = Ordering[Int].compare(x.major, y.major)
    if (majorComparison == 0) {
      x.descendingRanks zip y.descendingRanks collectFirst {
        case (xr, yr) if (xr.strength != yr.strength) =>
          Ordering[Int].compare(xr.strength, yr.strength)
      } match {
        case Some(comparison) => comparison
        case _                => 0
      }
    } else majorComparison
  }

  def of(board: Board, hand: Hand): Value = {
    val options = hand match {
      case Hand.Texas(cards) =>
        (board.cards ++ cards).combinations(5)

      case Hand.Omaha(cards) =>
        for {
          b <- board.cards.combinations(3)
          h <- cards.combinations(2)
        } yield b ++ h
    }

    options.map(fiveCard).max
  }

  private def fiveCard(cards: List[Card]): Value = {
    val descendingCards = cards.sortWith((x, y) => x.rank.strength < y.rank.strength).reverse

    val straightTop: Option[Rank] =
      descendingCards.map(_.rank).toSet.toList.sortWith(_.strength > _.strength) match {
        case list if list.length == 5 =>
          list match {
            case first :: second :: tail =>
              if (list.head.strength - 4 == list.last.strength) Some(first)
              else if (first == Ace && second == Five) Some(Five)
              else None
            case _ => None
          }
        case _ => None
      }

    val isFlush: Boolean = cards.map(_.suit).toSet.size == 1

    //list of (frequency_of_rank, rank) tuples sorted on descending frequency
    val rankList: List[(Int, Rank)] =
      descendingCards
        .map(_.rank)
        .foldLeft(Nil: List[(Int, Rank)])((acc, rank) =>
          acc match {
            case x :: xs => {
              val (count, prevRank) = x
              if (prevRank == rank) (count + 1, rank) :: xs
              else (1, rank) :: x :: xs
            }
            case _ => (1, rank) :: Nil
          }
        )
        .sortWith({ case ((x, _), (y, _)) => x < y })
        .reverse

    val attempts =
      StraightFlush.attempt(straightTop, isFlush) ::
        FourOfAKind.attempt(rankList) ::
        FullHouse.attempt(rankList) ::
        Flush.attempt(isFlush, rankList) ::
        Straight.attempt(straightTop) ::
        ThreeOfAKind.attempt(rankList) ::
        TwoPair.attempt(rankList) ::
        Pair.attempt(rankList) ::
        Nil

    attempts.collectFirstSome(identity).getOrElse(HighCard(cards))
  }

  private def rankSetToDescendingRankList(ranks: Set[Rank]): List[Rank] = {
    ranks.toList.sortWith((x, y) => { x.strength < y.strength }).reverse
  }

  final case class HighCard(set: Set[Rank]) extends Value {
    override def major: Int = 1
    override def descendingRanks: List[Rank] = rankSetToDescendingRankList(set)
  }

  object HighCard {
    def apply(cards: List[Card]): HighCard = {
      HighCard(cards.map(_.rank).toSet)
    }
  }

  final case class Pair(pair: Rank, kickers: Set[Rank]) extends Value {
    override def major: Int = 2
    override def descendingRanks: List[Rank] = pair :: rankSetToDescendingRankList(kickers)
  }

  object Pair {
    def attempt(descendingRankList: List[(Int, Rank)]): Option[Pair] =
      descendingRankList match {
        case (2, pair) :: xs => Some(Pair(pair, xs.map({ case (_, rank) => rank }).toSet))
        case _               => None
      }
  }

  final case class TwoPair(higher: Rank, lower: Rank, kicker: Rank) extends Value {
    override def major: Int = 3
    override def descendingRanks: List[Rank] = List(higher, lower, kicker)
  }

  object TwoPair {
    def attempt(descendingRankList: List[(Int, Rank)]): Option[TwoPair] =
      descendingRankList match {
        case (2, highPair) :: (2, lowPair) :: (1, kicker) :: Nil =>
          Some(TwoPair(highPair, lowPair, kicker))
        case _ => None
      }
  }

  final case class ThreeOfAKind(threeOfAKind: Rank, kickers: Set[Rank]) extends Value {
    override def major: Int = 4
    override def descendingRanks: List[Rank] = threeOfAKind :: rankSetToDescendingRankList(kickers)
  }

  object ThreeOfAKind {
    def attempt(descendingRankList: List[(Int, Rank)]): Option[ThreeOfAKind] =
      descendingRankList match {
        case (3, three) :: (1, highKicker) :: (1, lowKicker) :: Nil =>
          Some(ThreeOfAKind(three, Set(highKicker, lowKicker)))
        case _ => None
      }
  }

  final case class Straight(value: Rank) extends Value {
    override def major: Int = 5
    override def descendingRanks: List[Rank] = List(value)
  }

  object Straight {
    def attempt(straightTop: Option[Rank]): Option[Straight] =
      if (straightTop.isDefined) Some(Straight(straightTop.get)) else None
  }

  final case class Flush(ranks: Set[Rank]) extends Value {
    override def major: Int = 6
    override def descendingRanks: List[Rank] = rankSetToDescendingRankList(ranks)
  }

  object Flush {
    def attempt(isFlush: Boolean, descendingRankList: List[(Int, Rank)]): Option[Flush] =
      if (isFlush) Some(Flush(descendingRankList.map({ case (_, rank) => rank }).toSet))
      else None
  }

  final case class FullHouse(higher: Rank, lower: Rank) extends Value {
    override def major: Int = 7
    override def descendingRanks: List[Rank] = List(higher, lower)
  }

  object FullHouse {
    def attempt(descendingRankList: List[(Int, Rank)]): Option[FullHouse] =
      descendingRankList match {
        case (3, three) :: (2, two) :: Nil => Some(FullHouse(three, two))
        case _                             => None
      }
  }

  final case class FourOfAKind(fourOfAKind: Rank, kicker: Rank) extends Value {
    override def major: Int = 8
    override def descendingRanks: List[Rank] = List(fourOfAKind, kicker)
  }

  object FourOfAKind {
    def attempt(descendingRankList: List[(Int, Rank)]): Option[FourOfAKind] =
      descendingRankList match {
        case (4, four) :: (1, kicker) :: Nil => Some(FourOfAKind(four, kicker))
        case _                               => None
      }
  }

  final case class StraightFlush(value: Rank) extends Value {
    override def major: Int = 9
    override def descendingRanks: List[Rank] = List(value)
  }

  object StraightFlush {
    def attempt(straightTop: Option[Rank], isFlush: Boolean): Option[StraightFlush] =
      if (straightTop.isDefined && isFlush) Some(StraightFlush(straightTop.get)) else None
  }
}

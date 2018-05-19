package model

import java.lang.Math.abs

object Interval {
  def fromPitches(p1: Pitch, p2: Pitch): Interval = {
    val scaleDiff = ((p2.pitchClass.scalePosition - p1.pitchClass.scalePosition + 7) % 7) + 1
    val pitchDiff = (p2.pitchClass.noteVal - p1.pitchClass.noteVal + 12) % 12
    val pitchDiffOdd = (pitchDiff) % 2 == 1
    val rawInterval = (scaleDiff, pitchDiffOdd) match {
      case (1, _)     => PerfectUnison // 0
      case (2, true)  => MinorSecond // 1
      case (2, false) => MajorSecond // 2
      case (3, true)  => MinorThird // 3
      case (3, false) => MajorThird // 4
      case (4, true)  => PerfectFourth // 5
      case (4, false) => AugmentedFourth // 6
      case (5, false) => DiminishedFifth // 6
      case (5, true)  => PerfectFifth // 7
      case (6, false) => MinorSixth // 8
      case (6, true)  => MajorSixth // 9
      case (7, false) => MinorSeventh // 10
      case (7, true)  => MajorSeventh // 11
      case _          => ???
    }
    val augmentations = p2.alterationO.map(_.pitchChange).getOrElse(0) - p1.alterationO.map(_.pitchChange).getOrElse(0)
    augmentations match {
      case 0          => rawInterval
      case i if i > 0 => Iterator.iterate[Interval](rawInterval)(_.augmented).drop(i).next
      case i if i < 0 => Iterator.iterate[Interval](rawInterval)(_.diminished).drop(-i).next
    }
  }
}

sealed trait Interval {
  val prefix: String
  val name: String
  def diminished: Interval
  def augmented: Interval
  override def toString = s"${prefix} $name"
}

sealed trait Perfect extends Interval {
  val prefix = "Perfect"
}

sealed trait Minor extends Interval {
  val prefix = "Minor"
}

sealed trait Major extends Interval {
  val prefix = "Major"
}

sealed trait Diminished extends Interval {
  val prefix = "Diminished"
  def diminished = ???
}

sealed trait Augmented extends Interval {
  val prefix = "Augmented"
  def augmented = ???
}

sealed trait Unison extends Interval {
  val name = "Unison"
}

sealed trait Second extends Interval {
  val name = "Second"
}

sealed trait Third extends Interval {
  val name = "Third"
}

sealed trait Fourth extends Interval {
  val name = "Fourth"
}

sealed trait Fifth extends Interval {
  val name = "Fifth"
}

sealed trait Sixth extends Interval {
  val name = "Sixth"
}

sealed trait Seventh extends Interval {
  val name = "Seventh"
}

case object DiminishedUnison extends Unison with Diminished {
  val augmented = PerfectUnison
}

case object PerfectUnison extends Unison with Perfect {
  def diminished = DiminishedUnison
  val augmented = AugmentedUnison
}

case object AugmentedUnison extends Unison with Augmented {
  val diminished = PerfectUnison
}

case object DiminishedSecond extends Second with Diminished {
  val augmented = MinorSecond
}

case object MinorSecond extends Second with Minor {
  val diminished = DiminishedSecond
  val augmented = MajorSecond
}

case object MajorSecond extends Second with Major {
  val diminished = MinorSecond
  val augmented = AugmentedSecond
}

case object AugmentedSecond extends Second with Augmented {
  val diminished = MajorSecond
}

case object DiminishedThird extends Third with Diminished {
  val augmented = MinorThird
}

case object MinorThird extends Third with Minor {
  val diminished = DiminishedThird
  val augmented = MajorThird
}

case object MajorThird extends Third with Major {
  val diminished = MinorThird
  val augmented = AugmentedThird
}

case object AugmentedThird extends Third with Augmented {
  val diminished = MajorThird
}

case object DiminishedFourth extends Fourth with Diminished {
  val augmented = PerfectFourth
}

case object PerfectFourth extends Fourth with Perfect {
  val diminished = DiminishedFourth
  val augmented = AugmentedFourth
}

case object AugmentedFourth extends Fourth with Augmented {
  val diminished = PerfectFourth
}

case object DiminishedFifth extends Fifth with Diminished {
  val augmented = PerfectFifth
}

case object PerfectFifth extends Fifth with Perfect {
  val diminished = DiminishedFifth
  val augmented = AugmentedFifth
}

case object AugmentedFifth extends Fifth with Augmented {
  val diminished = PerfectFifth
}

case object DiminishedSixth extends Sixth with Diminished {
  val augmented = MinorSixth
}

case object MinorSixth extends Sixth with Minor {
  val diminished = DiminishedSixth
  val augmented = MajorSixth
}

case object MajorSixth extends Sixth with Major {
  val diminished = MinorSixth
  val augmented = AugmentedSixth
}

case object AugmentedSixth extends Sixth with Augmented {
  val diminished = MajorSixth
}

case object DiminishedSeventh extends Seventh with Diminished {
  val augmented = MinorSeventh
}

case object MinorSeventh extends Seventh with Minor {
  val diminished = DiminishedSeventh
  val augmented = MajorSeventh
}

case object MajorSeventh extends Seventh with Major {
  val diminished = MinorSeventh
  val augmented = AugmentedSeventh
}

case object AugmentedSeventh extends Seventh with Augmented {
  val diminished = MajorSeventh
}

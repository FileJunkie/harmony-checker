package analysis

import model._

object VerticalAnalyzer {
  def analyzeChord(chord: Chord) = {
    println(s"Analyzing chord $chord")
    val combinations = chord.pitches.combinations(2)
    combinations.foreach { combination =>
      val n1 = combination(0)
      val n2 = combination(1)
      val distance = n2.noteValue - n1.noteValue
      val interval = Interval.fromPitches(n1, n2)
      println(s"Distance between $n1 and $n2 is $distance semitones, interval is $interval (plus-minus octave or two)")
    }
  }
}

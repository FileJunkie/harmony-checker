package analysis

import model._

object VerticalAnalyzer {

  def analyzeChord(chord: PitchSet) = {
    println(s"Analyzing chord $chord")
    val combinations = chord.pitches.combinations(2)
    combinations.foreach { combination =>
      val n1 = combination(0)
      val n2 = combination(1)
      val distance = n2.noteValue - n1.noteValue
      val interval = Interval.fromPitches(n1, n2)
      println(s"Distance between $n1 and $n2 is $distance semitones, interval is $interval (plus-minus octave or two)")
    }
    val distinctPitches = chord.pitches.map(_.noOctave).distinct
    val bassNote = chord.pitches.sorted.head
    println(s"Distinct pitches are $distinctPitches, bass note is $bassNote")
    distinctPitches.size match {
      case 1 => println(s"Only one note is being played, it's ${distinctPitches.head}")
      case 2 | 3 | 4 => {
        println(s"Generating chords for $distinctPitches")
        val chords = Chord.generate(distinctPitches, bassNote.noOctave)
        println(chords)
        println(s"I think that this chord is ${chords.head}")
      }
      case _ => println("I don't recognize this chord")
    }
  }
}

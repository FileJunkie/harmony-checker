import imports.MusicXmlImporter

import analysis.VerticalAnalyzer
import model._

object HarmonyChecker {
  def main(args: Array[String]): Unit = {
    println("Harmony Checker started")
    args.foreach(arg => {
      val data = MusicXmlImporter.loadMusicXml(arg)
      data.zipWithIndex.foreach {
        case (measure, index) => {
          println(s"Measure ${index + 1}")
          measure.chords.zipWithIndex.foreach {
            case (chord, index) => {
              println(s"Chord ${index + 1} consists of pitches ${chord.pitches.mkString(", ")}")
              VerticalAnalyzer.analyzeChord(chord)
              println
            }
          }
        }
      }
    })
  }
}

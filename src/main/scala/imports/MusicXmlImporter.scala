package imports

import scala.xml.Elem
import scala.xml.Node
import scala.xml.factory.XMLLoader
import javax.xml.parsers.SAXParser
import javax.xml.parsers.SAXParserFactory
import model._
import scala.util.Try

object MusicXmlImporter {
  val customXMLLoader = new XMLLoader[Elem] {
    override def parser: SAXParser = {
      val f = SAXParserFactory.newInstance()
      f.setNamespaceAware(false)
      f.setFeature("http://xml.org/sax/features/validation", false)
      f.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false)
      f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
      f.setFeature("http://xml.org/sax/features/external-general-entities", false)
      f.setFeature("http://xml.org/sax/features/external-parameter-entities", false)
      f.newSAXParser()
    }
  }

  def nodeToNote(node: Node): (_, Note) = {
    val pitchNodeO = (node \\ "pitch").headOption
    val pitchO = pitchNodeO.map(pitchNode => {
      val pitchName = (pitchNode \\ "step").text
      val alterationO = Try((pitchNode \\ "alter").text.toInt).toOption
      val octave = (pitchNode \\ "octave").text.toInt

      val pitchClass = PitchClass.fromString(pitchName)
      val alteration = alterationO.flatMap(Alteration.fromPitchChange)
      Pitch(pitchClass, alteration, octave)
    })
    val duration = (node \\ "duration").text.toInt
    val voice = (node \\ "voice").text
    (voice, Note(duration, pitchO))
  }

  def isVoiceEmpty(voice: Seq[Note]): Boolean = voice.isEmpty || voice.forall(_.pitchO.isEmpty)

  def voicesToPitchSets(voices: Seq[Seq[Note]]): Seq[Seq[Pitch]] = {
    if (voices.isEmpty) return Nil
    val headsAndTails = voices.map(v => (v.head, v.tail))
    val shortestDuration = headsAndTails.map(_._1.duration).min
    val notesAndTails = headsAndTails.map {
      case (head, tail) => {
        val leftoverDuration = head.duration - shortestDuration
        if (leftoverDuration <= 0) {
          (head.pitchO, tail)
        } else {
          val fakeNote = Note(leftoverDuration, head.pitchO)
          val fakeTail = fakeNote +: tail
          (head.pitchO, fakeTail)
        }
      }
    }
    val pitches = notesAndTails.collect { case (Some(pitch), _) => pitch }.distinct.sorted.reverse
    val tails = notesAndTails.map(_._2).filterNot(isVoiceEmpty)
    pitches +: voicesToPitchSets(tails)
  }

  def parseMeasure(measure: Node) = {
    val voices = (measure \\ "note").map(nodeToNote).groupBy(_._1).map { case (k, v) => (k, v.map(x => x._2)) }.values.filterNot(isVoiceEmpty)
    voicesToPitchSets(voices.toSeq).map(Chord.apply)
  }

  // only one part for now
  def loadMusicXml(filename: String) = {
    val measures = (customXMLLoader.loadFile(filename) \\ "part").head \\ "measure"
    measures.map(parseMeasure).map(Measure.apply)
  }
}

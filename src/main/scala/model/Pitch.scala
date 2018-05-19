package model

case class Pitch(pitchClass: PitchClass, alterationO: Option[Alteration], octave: Int) extends Ordered[Pitch] {
  override def toString = pitchClass.toString + alterationO.map(_.toString).getOrElse("") + octave
  lazy val noteValue = 12 * octave + pitchClass.noteVal + alterationO.map(_.pitchChange).getOrElse(0)
  def compare(that: Pitch) = this.noteValue - that.noteValue
  lazy val noOctave = PitchNoOctave(pitchClass, alterationO)
}

case class PitchNoOctave(pitchClass: PitchClass, alterationO: Option[Alteration]) {
  override def toString = pitchClass.toString + alterationO.map(_.toString).getOrElse("")
  lazy val addMinorThird = {
    val newScalePosition = ((pitchClass.scalePosition - 1) + 2) % 7 + 1
    val newPitchClass = PitchClass.fromScalePosition(newScalePosition)
    val diff = (newPitchClass.noteVal - pitchClass.noteVal + 12) % 12
    diff match {
      case 3 => PitchNoOctave(newPitchClass, alterationO)
      case 4 => PitchNoOctave(newPitchClass, Alteration.flatten(alterationO))
    }
  }
  lazy val addMajorThird = {
    val newScalePosition = ((pitchClass.scalePosition - 1) + 2) % 7 + 1
    val newPitchClass = PitchClass.fromScalePosition(newScalePosition)
    val diff = (newPitchClass.noteVal - pitchClass.noteVal + 12) % 12
    diff match {
      case 3 => PitchNoOctave(newPitchClass, Alteration.sharpen(alterationO))
      case 4 => PitchNoOctave(newPitchClass, alterationO)
    }
  }
}

package model

case class Pitch(pitchClass: PitchClass, alterationO: Option[Alteration], octave: Int) extends Ordered[Pitch] {
  override def toString = pitchClass.toString + alterationO.map(_.toString).getOrElse("") + octave
  lazy val noteValue = 12 * octave + pitchClass.noteVal + alterationO.map(_.pitchChange).getOrElse(0)
  def compare(that: Pitch) = this.noteValue - that.noteValue
}

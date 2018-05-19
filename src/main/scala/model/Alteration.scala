package model

object Alteration {
  def fromPitchChange(pc: Int): Option[Alteration] = pc match {
    case -2 => Some(DoubleFlat)
    case -1 => Some(Flat)
    case 1  => Some(Sharp)
    case 2  => Some(DoubleSharp)
    case _  => None
  }
  def flatten(ao: Option[Alteration]): Option[Alteration] = ao match {
    case Some(Flat)        => Some(DoubleFlat)
    case None              => Some(Flat)
    case Some(Sharp)       => None
    case Some(DoubleSharp) => Some(Sharp)
  }
  def sharpen(ao: Option[Alteration]): Option[Alteration] = ao match {
    case Some(DoubleFlat) => Some(Flat)
    case Some(Flat)       => None
    case None             => Some(Sharp)
    case Some(Sharp)      => Some(DoubleSharp)
  }
}

sealed trait Alteration { val pitchChange: Int }
case object DoubleFlat extends Alteration { val pitchChange = -2; override def toString = "𝄫" }
case object Flat extends Alteration { val pitchChange = -1; override def toString = "♭" }
case object Sharp extends Alteration { val pitchChange = 1; override def toString = "♯" }
case object DoubleSharp extends Alteration { val pitchChange = 2; override def toString = "𝄪" }

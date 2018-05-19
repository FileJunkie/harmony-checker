package model

object Alteration {
  def fromPitchChange(pc: Int): Option[Alteration] = pc match {
    case -2 => Some(DoubleFlat)
    case -1 => Some(Flat)
    case 1  => Some(Sharp)
    case 2  => Some(DoubleSharp)
    case _  => None
  }
}

sealed trait Alteration { val pitchChange: Int }
case object DoubleFlat extends Alteration { val pitchChange = -2; override def toString = "𝄫" }
case object Flat extends Alteration { val pitchChange = -1; override def toString = "♭" }
case object Sharp extends Alteration { val pitchChange = 1; override def toString = "♯" }
case object DoubleSharp extends Alteration { val pitchChange = 2; override def toString = "𝄪" }

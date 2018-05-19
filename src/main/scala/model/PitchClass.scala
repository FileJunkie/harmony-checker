package model

object PitchClass {
  def fromString(s: String): PitchClass = s match {
    case "A" => A
    case "B" => B
    case "C" => C
    case "D" => D
    case "E" => E
    case "F" => F
    case "G" => G
  }
}

sealed trait PitchClass { val noteVal: Int }
case object A extends PitchClass { val noteVal = 9 }
case object B extends PitchClass { val noteVal = 11 }
case object C extends PitchClass { val noteVal = 0 }
case object D extends PitchClass { val noteVal = 2 }
case object E extends PitchClass { val noteVal = 4 }
case object F extends PitchClass { val noteVal = 5 }
case object G extends PitchClass { val noteVal = 7 }

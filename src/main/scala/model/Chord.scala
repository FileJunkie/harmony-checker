package model

object Chord {
  def generate(pitches: Seq[PitchNoOctave], bass: PitchNoOctave): Seq[Chord] = pitches.flatMap(p => generate(p, pitches, bass)).sortBy { case x => x.complexity }
  def generate(pitch: PitchNoOctave, pitches: Seq[PitchNoOctave], bass: PitchNoOctave): Seq[Chord] = {
    val minor = new MinorChord(pitch, pitches, bass)
    val major = new MajorChord(pitch, pitches, bass)
    val dim = new DiminishedChord(pitch, pitches, bass)
    val aug = new AugmentedChord(pitch, pitches, bass)
    val sevM = new MajorSeventhChord(pitch, pitches, bass)
    val sevm = new MinorSeventhChord(pitch, pitches, bass)
    val sevD = new DominantSeventhChord(pitch, pitches, bass)
    val sevDim = new DiminishedChord(pitch, pitches, bass)
    val sevHDim = new HalfDiminishedSeventhChord(pitch, pitches, bass)
    val sevMinMaj = new MinorMajorSeventhChord(pitch, pitches, bass)
    val sevAug = new AugmentedMajorSeventhChord(pitch, pitches, bass)
    Seq(minor, major, dim, aug, sevM, sevm, sevD, sevDim, sevHDim, sevMinMaj, sevAug).filter(c => c.seventh.isEmpty || pitches.contains(c.seventh.get))
  }
}

trait Chord {
  val baseComplexity: Int
  val root: PitchNoOctave
  val provided: Seq[PitchNoOctave]
  val bass: PitchNoOctave

  val third: PitchNoOctave
  val fifth: PitchNoOctave
  val seventh: Option[PitchNoOctave]
  val name: String
  lazy val allNotes = Seq(Some(root), Some(third), Some(fifth), seventh).flatten
  lazy val missingCount = allNotes.size - allNotes.intersect(provided).size
  lazy val thirdMissing = if (!provided.contains(third)) { Some("third") } else { None }
  lazy val fifthMissing = if (!provided.contains(fifth)) { Some("fifth") } else { None }
  lazy val missings = Seq(thirdMissing, fifthMissing).flatten.mkString(", ") match {
    case "" => None
    case x  => Some(x)
  }
  lazy val missingText = missings.map(m => s" with $m missing").getOrElse("")
  lazy val withBassText = if (root != bass) { s" with $bass in bass" } else { "" }
  override def toString = s"$name$missingText$withBassText ($root $third $fifth ${seventh.getOrElse("")} for full chord) $complexity"
  lazy val extraTonePenalty = if (provided.exists(t => !allNotes.contains(t))) { 1000 } else { 0 }
  lazy val complexity = baseComplexity + thirdMissing.map(x => 20).getOrElse(0) + fifthMissing.map(x => 5).getOrElse(0) + (if (root != bass) { 1 } else { 0 }) + extraTonePenalty
}

class MinorChord(val root: PitchNoOctave, val provided: Seq[PitchNoOctave], val bass: PitchNoOctave) extends Chord {
  val baseComplexity = 1
  val name = s"Minor $root chord"
  val third = root.addMinorThird
  val fifth = third.addMajorThird
  val seventh = None
}

class MajorChord(val root: PitchNoOctave, val provided: Seq[PitchNoOctave], val bass: PitchNoOctave) extends Chord {
  val baseComplexity = 1
  val name = s"Major $root chord"
  val third = root.addMajorThird
  val fifth = third.addMinorThird
  val seventh = None
}

class DiminishedChord(val root: PitchNoOctave, val provided: Seq[PitchNoOctave], val bass: PitchNoOctave) extends Chord {
  val baseComplexity = 50
  val name = s"Diminished $root chord"
  val third = root.addMinorThird
  val fifth = third.addMinorThird
  val seventh = None
}

class AugmentedChord(val root: PitchNoOctave, val provided: Seq[PitchNoOctave], val bass: PitchNoOctave) extends Chord {
  val baseComplexity = 50
  val name = s"Augmented $root chord"
  val third = root.addMajorThird
  val fifth = third.addMajorThird
  val seventh = None
}

class MajorSeventhChord(val root: PitchNoOctave, val provided: Seq[PitchNoOctave], val bass: PitchNoOctave) extends Chord {
  val baseComplexity = 500
  val name = s"Major seventh $root chord"
  val third = root.addMajorThird
  val fifth = third.addMinorThird
  val seventh = Some(fifth.addMajorThird)
}

class MinorSeventhChord(val root: PitchNoOctave, val provided: Seq[PitchNoOctave], val bass: PitchNoOctave) extends Chord {
  val baseComplexity = 500
  val name = s"Minor seventh $root chord"
  val third = root.addMinorThird
  val fifth = third.addMajorThird
  val seventh = Some(fifth.addMinorThird)
}

class DominantSeventhChord(val root: PitchNoOctave, val provided: Seq[PitchNoOctave], val bass: PitchNoOctave) extends Chord {
  val baseComplexity = 500
  val name = s"Dominant seventh $root chord"
  val third = root.addMajorThird
  val fifth = third.addMinorThird
  val seventh = Some(fifth.addMinorThird)
}

class DiminishedSeventhChord(val root: PitchNoOctave, val provided: Seq[PitchNoOctave], val bass: PitchNoOctave) extends Chord {
  val baseComplexity = 500
  val name = s"Diminished seventh $root chord"
  val third = root.addMinorThird
  val fifth = third.addMinorThird
  val seventh = Some(fifth.addMinorThird)
}

class HalfDiminishedSeventhChord(val root: PitchNoOctave, val provided: Seq[PitchNoOctave], val bass: PitchNoOctave) extends Chord {
  val baseComplexity = 500
  val name = s"Half-diminished seventh $root chord"
  val third = root.addMinorThird
  val fifth = third.addMinorThird
  val seventh = Some(fifth.addMajorThird)
}

class MinorMajorSeventhChord(val root: PitchNoOctave, val provided: Seq[PitchNoOctave], val bass: PitchNoOctave) extends Chord {
  val baseComplexity = 500
  val name = s"Minor major seventh $root chord"
  val third = root.addMinorThird
  val fifth = third.addMajorThird
  val seventh = Some(fifth.addMajorThird)
}

class AugmentedMajorSeventhChord(val root: PitchNoOctave, val provided: Seq[PitchNoOctave], val bass: PitchNoOctave) extends Chord {
  val baseComplexity = 500
  val name = s"Augmented major seventh $root chord"
  val third = root.addMajorThird
  val fifth = third.addMajorThird
  val seventh = Some(fifth.addMinorThird)
}

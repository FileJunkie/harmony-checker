import imports.MusicXmlImporter

object HarmonyChecker {
  def main(args: Array[String]): Unit = {
    println("Harmony Checker started")
    args.foreach(arg => {
      println(MusicXmlImporter.loadMusicXml(arg))
    })
  }
}

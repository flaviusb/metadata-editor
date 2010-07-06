import sbt._
import Process._

class ScalaMetadataEditorProject(info: ProjectInfo) extends DefaultProject(info)
{
  def extraResources = "LICENSE" +++ "COPYING" +++ "README.md"
  override def mainResources = super.mainResources +++ extraResources
  
  val jena = "com.hp.hpl.jena" % "jena" % "2.6.2"
  val scalaSwing = "org.scala-lang" % "scala-swing" % "2.8.0.RC6"

  override def packageOptions: Seq[PackageOption] = MainClass("org.cellml.metadata_editor.MetadataEditor") :: Nil
  //override def shouldCheckOutputDirectories = false
  //override def updateAction = task { None }

  //lazy val main  = project(".", "foo", new MainProject(_))
  //lazy val tests = project(".", "foo", new MainProject(_))
  lazy val hi = task { println("Hello World"); None }

  lazy val literatedocs = task {
    val p = "src" / "main" / "scala" / "org" / "cellml" / "metadata_editor"
    val f = p * "*.scala"
    val files = f.get.map(_.asFile.getName())
    val command = Seq("rocco", "-o", "../../../../../../docs", "-l", "scala", "-c", "//") ++ files.toSeq 
    (new java.lang.ProcessBuilder(command : _*) directory p.asFile) ! ;
    None
  }
}

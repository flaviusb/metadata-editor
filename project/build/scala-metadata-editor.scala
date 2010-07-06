import sbt._
import Process._

class ScalaMetadataEditorProject(info: ProjectInfo) extends DefaultProject(info)
{
  //override def shouldCheckOutputDirectories = false
  //override def updateAction = task { None }

  //lazy val main  = project(".", "foo", new MainProject(_))
  //lazy val tests = project(".", "foo", new MainProject(_))
  lazy val hi = task { println("Hello World"); None }

  lazy val literatedocs = task {
    val p = "src" / "main" / "scala" / "org" / "cellml" / "metadata_editor"
    val f = p * "*.scala"
    println(f)
    def cmd(a: String): java.lang.ProcessBuilder = (new java.lang.ProcessBuilder("rocco", "-o", "../../../../../../docs", "-l", "scala", "-c", "//", a) directory p.asFile)
    //val cmd = <x>bash -c &quot;cd {p} &amp;&amp; rocco -o ../../../../../../docs -l scala -c // *.scala&quot;</x>
    //println(cmd)
    f.get.foreach(a => (cmd(a.asFile.getName()) !))
    None
  }
}

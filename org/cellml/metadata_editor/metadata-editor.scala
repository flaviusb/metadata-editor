package org.cellml.metadata_editor

import scala.swing._
import java.io._
import com.hp.hpl.jena.rdf.arp._
import com.hp.hpl.jena.rdf.model._

object MetadataEditor extends SimpleSwingApplication {
  case class editor(schema: String) extends FlowPanel {
    contents += new TextField { text = schema }
  }
  def editors(file: File): Panel = {
    var fis = new FileInputStream(file)
    var m = ModelFactory.createDefaultModel()
    var arp = m.getReader()
    arp.setProperty("embedding", true)
    var cmetans = "http://www.cellml.org/metadata/1.0"
    arp.read(m, fis, cmetans)
    m.write(System.out)
    
    new FlowPanel(controls: _*)
  }
  val types = List("Author", "Citation", "Change History")
  val controls: List[editor] = types.map(a => editor(a))
  def top = new MainFrame {
    title = "Metadata Editor"
    //contents = new FlowPanel(controls: _*)
    contents = Button("Open CellML model") {
      val f = new FileChooser()
      f.showOpenDialog(this.contents(0))
      if (f.selectedFile != null)
        this.contents = editors(f.selectedFile);
    } 
  }
}

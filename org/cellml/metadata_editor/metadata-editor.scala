package org.cellml.metadata_editor

import scala.swing._
import scala.swing.event._
import java.io._
import com.hp.hpl.jena.rdf.arp._
import com.hp.hpl.jena.rdf.model._

object MetadataEditor extends SimpleSwingApplication {
  case class editor(schema: String, value: String) extends FlowPanel {
    contents += new Label(schema)
    contents += new TextField { text = value }
  }
  def editors(file: File): Panel = {
    var fis = new FileInputStream(file)
    var m = ModelFactory.createDefaultModel()
    var arp = m.getReader()
    arp.setProperty("embedding", true)
    var cmetans = "http://www.cellml.org/metadata/1.0"
    arp.read(m, fis, cmetans)
    //m.createResource("").addProperty(m.createProperty("http://foo.com/a#P"), "fhsdhflwhdlfhwsdfhwdj")
    //var arw = m.getWriter("RDF/XML-ABBREV")
    //arw.setProperty("relativeURIs", "same-document, absolute, relative, parent")
    //arw.write(m, System.out, cmetans)
    val aboutModel = m.getResource("")
    
    val controls: List[editor] = types.map(a => editor(a._1, a._2("getter")(m)))
    new FlowPanel(controls: _*)
  }
  def getVCardGivenName(mod: Model, vcard: {def getProperty(a: Property): Statement}): String = {
    vcard.getProperty(mod.createProperty("http://www.w3.org/2001/vcard-rdf/3.0#N"))
      .getProperty(mod.createProperty("http://www.w3.org/2001/vcard-rdf/3.0#Given"))
        .getString()
  }
  val types = List("Author" -> Map("getter" -> ((mod: Model) => {
    val x = mod.getResource("").getProperty(mod.createProperty("http://purl.org/dc/elements/1.1/#creator"))
    var y: String = ""
    try {
      y = getVCardGivenName(mod, x.getBag().iterator.nextNode().as(classOf[com.hp.hpl.jena.rdf.model.Resource]))
    } catch {
      case (e:NullPointerException) => y = getVCardGivenName(mod, x)
    }
    y
  })),
    "Citation" -> Map("getter" -> ((mod: Model) => "foo")), "Change History" ->  Map("getter" -> ((mod: Model) => "roo")))
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

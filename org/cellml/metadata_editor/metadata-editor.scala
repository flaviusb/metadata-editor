package org.cellml.metadata_editor

import scala.swing._
import scala.swing.event._
import java.io._
import com.hp.hpl.jena.rdf.arp._
import com.hp.hpl.jena.rdf.model.{Model => Model, Alt => Alt, Bag => Bag, Seq => JSeq, Container => JContainer, RDFNode => RDFNode, ModelFactory => ModelFactory, Property => Property, Statement => Statement, Resource => Resource}

object MetadataEditor extends SimpleSwingApplication {
  implicit def container2seq(thing: JContainer): Seq[RDFNode] = {
    var f: Seq[RDFNode] = Seq()
    var iter = thing.iterator
    while(iter.hasNext)
      f = f :+ iter.next()
    f
  }

  case class editor(schema: String, value: String) extends FlowPanel {
    contents += new Label(schema)
    contents += new TextField { text = value }
  }
  case class ResourceEditor(root: Resource, predicate: Property) extends FlowPanel {
    def get: String = root.getProperty(predicate).getString()
    def set(value: String): Unit = root.addProperty(predicate, value)

    var inneredit = new TextField { text = get }
    contents += inneredit
    listenTo(inneredit)
    reactions += {
      case EditDone(inneredit) => set(inneredit.text)
    }
  }
  case class BagEditor(root: Bag, builder: Resource => FlowPanel) extends FlowPanel {
    root.foreach(a => contents += builder(a.as(classOf[com.hp.hpl.jena.rdf.model.Resource])))
  }
  case class CompoundEditor(root: Resource, builder: Seq[Resource => FlowPanel]) extends FlowPanel {
    builder.foreach(a => contents += a(root))
  }
  def editors(file: File): Panel = {
    var fis = new FileInputStream(file)
    var m = ModelFactory.createDefaultModel()
    var arp = m.getReader()
    arp.setProperty("embedding", true)
    var cmetans = "http://www.cellml.org/metadata/1.0"
    arp.read(m, fis, "")
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
    val x = mod.getResource("").getProperty(mod.createProperty("http://purl.org/dc/elements/1.1/creator"))
    var y: String = ""
    try {
      y = getVCardGivenName(mod, x.getBag().iterator.nextNode().as(classOf[com.hp.hpl.jena.rdf.model.Resource]))
    } catch {
      case (e:java.util.NoSuchElementException) => y = getVCardGivenName(mod, x)
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

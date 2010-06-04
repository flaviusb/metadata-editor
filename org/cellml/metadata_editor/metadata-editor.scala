package org.cellml.metadata_editor

import scala.swing._
import scala.swing.event._
import scala.xml._
import scala.xml.parsing.ConstructingParser
import java.io._
import com.hp.hpl.jena.rdf.arp._
import com.hp.hpl.jena.rdf.model.{Model => Model, Alt => Alt, Bag => Bag, Seq => JSeq, Container => JContainer, RDFNode => RDFNode, ModelFactory => ModelFactory, Property => Property, Statement => Statement, Resource => Resource}

object MetadataEditor extends SimpleSwingApplication {
  type propertyable = { def getProperty(a: Property): Statement; def addProperty(a: Property, b: String): Resource }
  class stmt2prop(stmt: Statement) {
    def getProperty(a: Property): Statement = stmt.getProperty(a)
    def addProperty(a: Property, b: String) = stmt.getResource().addProperty(a, b)
  }
  type propertyish = { def getProperty(a: Property): Statement }
  implicit def container2seq(thing: JContainer): Seq[RDFNode] = {
    var f: Seq[RDFNode] = Seq()
    var iter = thing.iterator
    while(iter.hasNext)
      f = f :+ iter.next()
    f
  }
  implicit def statement2propertyable(stmt: Statement): propertyable = new stmt2prop(stmt)
  case class editor(schema: String, value: String) extends FlowPanel {
    contents += new Label(schema)
    contents += new TextField { text = value }
  }
  case class ResourceEditor(root: propertyable, predicate: Property) extends FlowPanel {
    def get: String = root.getProperty(predicate).getString()
    def set(value: String): Unit = root.getProperty(predicate).changeObject(value)

    var inneredit = new TextField { text = get }
    contents += inneredit
    listenTo(inneredit)
    reactions += {
      case EditDone(inneredit) => set(inneredit.text)
    }
  }
  case class BagEditor(root: Bag, builder: propertyable => FlowPanel) extends FlowPanel {
    root.foreach(a => contents += builder(a.as(classOf[com.hp.hpl.jena.rdf.model.Resource])))
  }
  case class CompoundEditor(root: propertyable, builder: Seq[propertyable => FlowPanel]) extends FlowPanel {
    builder.foreach(a => contents += a(root))
  }
  def labeledtext(label: String, predicate: Property)(root: propertyable): FlowPanel = {
    new FlowPanel(new Label(label), ResourceEditor(root, predicate))
  }
  def editors(file: File): Panel = {
    var fis = new FileInputStream(file)
    var m = ModelFactory.createDefaultModel()
    var arp = m.getReader()
    arp.setProperty("embedding", true)
    var cmetans = "http://www.cellml.org/metadata/1.0"
    arp.read(m, fis, "")
    val aboutModel = m.getResource("")
    val vcn = m.createProperty("http://www.w3.org/2001/vcard-rdf/3.0#N")
    val vcg = m.createProperty("http://www.w3.org/2001/vcard-rdf/3.0#Given")
    val vcf = m.createProperty("http://www.w3.org/2001/vcard-rdf/3.0#Family")
    val dcc = m.createProperty("http://purl.org/dc/elements/1.1/creator")
    val controls = CompoundEditor(aboutModel, Seq(a => CompoundEditor(a.getProperty(dcc), Seq(
      b => CompoundEditor(b.getProperty(vcn), Seq(
        labeledtext("Given Name: ", vcg), labeledtext("Family Name: ", vcf)
      ))
    ))))
    new FlowPanel(controls, Button("Save metadata to file") {
      def stripRDF(el: Node): Node = 
        el match {
          case <rdf>{inner @ _*}</rdf> => new Group(Seq[Node]())
          case a:Elem => (new Elem(a.prefix, a.label, a.attributes, a.scope, a.descendant.map(stripRDF) : _*))
          case b:Node => (b)
        }
      
      val p = ConstructingParser.fromFile(file, true)
      var d: xml.Document = stripRDF(p.document)
      
      var arw = m.getWriter("RDF/XML-ABBREV")
      arw.setProperty("relativeURIs", "same-document, absolute, relative, parent")
      arw.setProperty("allowBadURIs", "true")
      arw.write(m, System.out, "")
    })
  }
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

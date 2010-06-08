package org.cellml.metadata_editor

import scala.swing._
import scala.swing.event._
import scala.xml._
import scala.xml.parsing.ConstructingParser
import java.io._
import com.hp.hpl.jena.rdf.arp._
import com.hp.hpl.jena.rdf.model.{Model, Alt, Bag, Seq => JSeq, Container => JContainer, RDFNode, ModelFactory, Property, Statement, Resource}

object MetadataEditor extends SimpleSwingApplication {
  type propertyable = {
    def getProperty(a: Property): Statement
    def addProperty(a: Property, b: String): Resource
    def addProperty(a: Property, b: RDFNode): Resource
    def hasProperty(a: Property): Boolean
    def removeAll(a: Property): Resource
  }
  type thingable = Component with propertyable
  class stmt2prop(stmt: Statement) {
    def getProperty(a: Property): Statement = stmt.getProperty(a)
    def addProperty(a: Property, b: String) = stmt.getResource().addProperty(a, b)
    def addProperty(a: Property, b: RDFNode) = stmt.getResource().addProperty(a, b)
    def hasProperty(a: Property) = stmt.getResource().hasProperty(a)
    def removeAll(a: Property) = stmt.getResource().removeAll(a)
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
  case class Interconvertable(root: propertyable, discriminator: propertyable => Int, converters: Seq[Seq[propertyable => Unit]], things: Seq[(String, (propertyable => FlowPanel))]) extends FlowPanel {
    val changemenu = new ListView(things.map(_._1))
    listenTo(changemenu.selection)
    reactions += {
      case ListSelectionChanged(changemenu, a, _) => {
        contents.clear()
        converters(discriminator(root))(changemenu.selection.indices.toSeq(0))(root)
        refresh
      }
    }
    def refresh: Unit = { contents.clear(); contents += things(discriminator(root))._2(root); contents += changemenu; }
    refresh
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
    val vcfn = m.createProperty("http://www.w3.org/2001/vcard-rdf/3.0#FN")
    val vcg = m.createProperty("http://www.w3.org/2001/vcard-rdf/3.0#Given")
    val vcf = m.createProperty("http://www.w3.org/2001/vcard-rdf/3.0#Family")
    val vco = m.createProperty("http://www.w3.org/2001/vcard-rdf/3.0#Other")
    val dcc = m.createProperty("http://purl.org/dc/elements/1.1/creator")
    def vcfn2vcp(root: propertyable): Unit = {
      var str = root.getProperty(vcfn).getString()
      //Seq(root.getProperty(vcg), root.getProperty(vcf), vcn).map(root.removeAll(_))
      root.removeAll(vcfn)
      var intermediate = m.createResource()
      root.addProperty(vcn, intermediate)
      //val ss = str.split("""\s+""")
      //val (first, last) = (ss(0), (1 to ss.length).toList :/ +)
      val fml = """(.*) (.*) (.*)""".r
      val fl = """(.*) (.*)""".r
      str match {
        case fml(first, other, last) => {
          intermediate.addProperty(vcg, first)
          intermediate.addProperty(vco, other)
          intermediate.addProperty(vcf, last)
        }
        case fl(first, last) => {
          intermediate.addProperty(vcg, first)
          intermediate.addProperty(vcf, last)
        }
        case _ => {
          intermediate.addProperty(vcg, str)
          intermediate.addProperty(vcf, "")
        }
      }
    }
    def vcp2vcfn(root: propertyable): Unit = {
      var str = root.getProperty(vcn).getProperty(vcg).getString() + " " +
        root.getProperty(vcn).getProperty(vco).getString() + " " + root.getProperty(vcn).getProperty(vcf).getString()
      Seq(vcg, vco, vcf).map(root.getProperty(vcn).removeAll(_))
      root.removeAll(vcn)
      root.addProperty(vcfn, str)
    }
    def vcfnorn(root: propertyable): Int = {
      if (root.hasProperty(vcfn)) 1
      else 0
    }
    val controls = CompoundEditor(aboutModel, Seq(a => Interconvertable(a.getProperty(dcc), vcfnorn, Seq(Seq((_) => Unit, vcp2vcfn _), Seq(vcp2vcfn _, (_) => Unit)),
      Seq(("vcard:N",
      b => CompoundEditor(b.getProperty(vcn), Seq(
        labeledtext("Given Name: ", vcg), labeledtext("Other Name: ", vco), labeledtext("Family Name: ", vcf)
      ))),
      ("vcard:FN",
        labeledtext("Full Name: ", vcfn)
      ))
    )))
    new FlowPanel(controls, Button("Save metadata to file") {
      def stripRDF(el: Node): Node = 
        el match {
          case b: Text => (b)
          case <rdf>{inner @ _*}</rdf> => new Text("")
          case <RDF>{inner @ _*}</RDF> => new Text("")
          case a: Elem => (new Elem(a.prefix, a.label, a.attributes, a.scope, a.descendant.map(stripRDF) : _*))
          case b: Node => (b)
        }
      val p = ConstructingParser.fromFile(file, true)
      var d: Node = stripRDF(p.document.docElem)
      
      var arw = m.getWriter("RDF/XML-ABBREV")
      arw.setProperty("relativeURIs", "same-document, absolute, relative, parent")
      arw.setProperty("allowBadURIs", "true")
      var capture = new StringWriter()
      capture.write("<fakeroot>");
      arw.write(m, capture, "")
      capture.write("</fakeroot>")
      var xm = XML.loadString(capture.toString())
      val finaldoc = new Elem(d.prefix, d.label, d.attributes, d.scope, (d.child ++ xm.child): _*) 
      val fo = new FileChooser()
      fo.showSaveDialog(controls)
      if (fo.selectedFile != null) 
        XML.save(fo.selectedFile.toString(), finaldoc);
    })
  }
  def top = new MainFrame {
    title = "Metadata Editor"
    contents = Button("Open CellML model") {
      val f = new FileChooser()
      f.showOpenDialog(this.contents(0))
      if (f.selectedFile != null)
        this.contents = editors(f.selectedFile);
    } 
  }
}

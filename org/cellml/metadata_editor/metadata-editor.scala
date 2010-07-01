package org.cellml.metadata_editor

import scala.swing._
import javax.swing.BoxLayout
import javax.swing.border._
import scala.swing.event._
import scala.xml._
import scala.xml.parsing.ConstructingParser
import java.io._
import com.hp.hpl.jena.rdf.arp._
import com.hp.hpl.jena.rdf.model.{Model, Alt, Bag, Seq => JSeq, Container => JContainer, RDFNode, ModelFactory, Property, Statement, Resource}

object MetadataEditor extends SimpleSwingApplication {
  import JenaWrapper._
  case class editor(schema: String, value: String) extends FlowPanel {
    contents += new Label(schema)
    contents += new TextField { text = value }
  }
  object eph extends stringable {
    def getString(): String = ""
  }
  case class ResourceEditor(root: propertyable, predicate: Property) extends FlowPanel {
    def get: String = ((for(s <- root.getProperty(predicate))
                         yield {
                           // This could be either a literal or an URI Resource
                           if(s.isInstanceOf[safeResWrapper]) s.as(classOf[Resource]).orNull.getURI()
                           else s.getString()
                         }) getOrElse "")
    def set(value: String): Unit = if(showing) { root.removeAll(predicate); root.addProperty(predicate, value) }

    var inneredit = new TextField(15) { text = get }
    contents += inneredit
    listenTo(inneredit)
    reactions += {
      case EditDone(inneredit) => set(inneredit.text)
    }
  }
  case class BigResourceEditor(root: propertyable, predicate: Property) extends FlowPanel {
    def get: String = ((for(s <- root.getProperty(predicate))
                         yield {
                           // This could be either a literal or an URI Resource
                           if(s.isInstanceOf[safeResWrapper]) s.as(classOf[Resource]).orNull.getURI()
                           else s.getString()
                         }) getOrElse "")
    def set(value: String): Unit = if(showing) { root.removeAll(predicate); root.addProperty(predicate, value) }

    var inneredit = new TextArea(6, 95) { text = get }
    contents += inneredit
    listenTo(inneredit)
    reactions += {
      case ValueChanged(inneredit: TextComponent) => set(inneredit.text)
    }
  }
  case class URIResourceEditor(root: propertyable, predicate: Property) extends FlowPanel {
    def get: String = ((for(s <- root.getProperty(predicate))
                         yield {
                           // This could be either a literal or an URI Resource
                           if(s.isInstanceOf[safeResWrapper]) s.as(classOf[Resource]).orNull.getURI()
                           else s.getString()
                         }) getOrElse "")
    def set(value: String): Unit = if(showing) { root.removeAll(predicate); root.addProperty(predicate, root.getModel().orNull.createResource(value)) }

    var inneredit = new TextField(15) { text = get }
    contents += inneredit
    listenTo(inneredit)
    reactions += {
      case EditDone(inneredit) => set(inneredit.text)
    }
  }
  case class ContEditor[A <: JContainer](superroot: propertyable, superpred: Property, superbuilder: Unit => A,
    var root: A, builder: propertyable => FlowPanel, border0: Border = Swing.TitledBorder(Swing.LineBorder(new Color(3010101).darker.darker.darker), "Container")) extends ColumnPanel {
    def rebuild: Unit = {
      contents.clear()
      root.foreach(a => {
        contents += new FlowPanel(builder(a.as(classOf[com.hp.hpl.jena.rdf.model.Resource])),
        Button(" - ") { 
          // This is used because we can't remove RDFNodes from a JContainer due to Jena brokenness, so we have to regenerate the containers instead
          val transfer: Seq[RDFNode] = root
          superroot.removeAll(superpred)
          root = superbuilder()
          superroot.addProperty(superpred, root)
          transfer.foreach(b => if (!b.equals(a)) root.add(b))
          rebuild
          revalidate
          repaint
        })
      })
      contents += Button(" + ") { 
        root.add(root.as(classOf[Resource]).getModel().createResource())
        rebuild
        revalidate
        repaint
      }
    }
    rebuild
    border = border0
  }
  case class CompoundEditor(root: propertyable, builder: Seq[propertyable => FlowPanel], border0: Border = new EmptyBorder(0, 0, 0, 0)) extends FlowPanel {
    builder.foreach(a => contents += a(root))
    border = border0
  }
  case class CompoundColumnEditor(root: propertyable, builder: Seq[propertyable => FlowPanel], border0: Border = new EmptyBorder(0, 0, 0, 0)) extends ColumnPanel {
    builder.foreach(a => contents += a(root))
    border = border0
  }
  case class Interconvertable(root: propertyable, discriminator: propertyable => Int, converters: Seq[Seq[propertyable => Unit]], things: Seq[(String, (propertyable => FlowPanel))]) extends FlowPanel {
    val changemenu = new ListView(things.map(_._1))
    listenTo(changemenu.selection)
    reactions += {
      case ListSelectionChanged(changemenu, a, _) => {
        contents.clear()
        repaint
        converters(discriminator(root))(changemenu.selection.indices.toSeq(0))(root)
        refresh
      }
    }
    def refresh: Unit = {
      contents.clear();
      val item = discriminator(root)
      contents += things(item)._2(root)
      contents += changemenu
      deafTo(changemenu.selection)
      changemenu.selectIndices(item)
      listenTo(changemenu.selection)
      revalidate
      repaint
    }
    refresh
  }
  def labeledtext(label: String, predicate: Property)(root: propertyable): FlowPanel = {
    new FlowPanel(new Label(label), ResourceEditor(root, predicate))
  }
  def labeledbigtext(label: String, predicate: Property)(root: propertyable): FlowPanel = {
    new FlowPanel(new Label(label), BigResourceEditor(root, predicate))
  }
  def labeleduritext(label: String, predicate: Property)(root: propertyable): FlowPanel = {
    new FlowPanel(new Label(label), URIResourceEditor(root, predicate))
  }
  def editors(file: File): Component = {
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
    val vcemail = m.createProperty("http://www.w3.org/2001/vcard-rdf/3.0#EMAIL")
    val vctel = m.createProperty("http://www.w3.org/2001/vcard-rdf/3.0#TEL")
    val vcorg = m.createProperty("http://www.w3.org/2001/vcard-rdf/3.0#ORG")
    val vcorgn = m.createProperty("http://www.w3.org/2001/vcard-rdf/3.0#Orgname")
    val vcorgu = m.createProperty("http://www.w3.org/2001/vcard-rdf/3.0#Orgunit")
    val dcc = m.createProperty("http://purl.org/dc/elements/1.1/creator")
    val dcco = m.createProperty("http://purl.org/dc/elements/1.1/contributor")
    val dcpub = m.createProperty("http://purl.org/dc/elements/1.1/publisher")
    val dcrights = m.createProperty("http://purl.org/dc/elements/1.1/rights")
    val dctcreated = m.createProperty("http://purl.org/dc/terms/created")
    val dctdate = m.createProperty("http://purl.org/dc/terms/W3CDTF")
    val dctabstract = m.createProperty("http://purl.org/dc/terms/abstract")
    val rdfval = m.createProperty("http://www.w3.org/1999/02/22-rdf-syntax-ns#value")
    val rdftype = m.createProperty("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
    def vcfn2vcp(root: propertyable): Unit = {
      var str = (root.getProperty(vcfn) getOrElse eph).getString()
      root.removeAll(vcfn)
      var intermediate = m.createResource()
      root.addProperty(vcn, intermediate)
      val fml = """([^ ]*) (.*) (.*)""".r
      val fl = """([^ ]*) (.*)""".r
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
      var str = (for (r <- root.getProperty(vcn))
        yield {
          val res = (r.getProperty(vcg) getOrElse eph).getString() + " " +
          (r.getProperty(vco) getOrElse eph).getString() + " " +
          (r.getProperty(vcf) getOrElse eph).getString()
          Seq(vcg, vco, vcf).map(r.removeAll(_))
          res
        }) getOrElse ""
      root.removeAll(vcn)
      root.addProperty(vcfn, str)
    }
    def vcfnorn(root: propertyable): Int = {
      if (root.hasProperty(vcfn)) 1
      else 0
    }
    def getOrMakeProp(root: propertyable, prop: Property): propertyable = {
      if (!root.hasProperty(prop))
        root.addProperty(prop, m.createResource())
      root.getProperty(prop) orNull
    }
    def seq2resource(prop: Property)(root: propertyable): Unit = {
      // In this case, we just take the first resource in the container; this becomes *the* resource.
      val f: Option[JSeq] = getOrMakeProp(root, prop).as(classOf[JSeq])
      val nv: RDFNode = f.orNull.iterator.next().as(classOf[RDFNode])
      root.removeAll(prop)
      root.addProperty(prop, nv)
    }
    def bag2resource(prop: Property)(root: propertyable): Unit = {
      // In this case, we just take the first resource in the container; this becomes *the* resource.
      val f: Option[Bag] = getOrMakeProp(root, prop).as(classOf[Bag])
      val nv: RDFNode = f.orNull.iterator.next().as(classOf[RDFNode])
      root.removeAll(prop)
      root.addProperty(prop, nv)
    }
    def alt2resource(prop: Property)(root: propertyable): Unit = {
      // In this case, we just take the first resource in the container; this becomes *the* resource.
      val f: Option[Alt] = getOrMakeProp(root, prop).as(classOf[Alt])
      val nv: RDFNode = f.orNull.iterator.next().as(classOf[RDFNode])
      root.removeAll(prop)
      root.addProperty(prop, nv)
    }
    def resource2container[T <: JContainer](builder: Unit => T)(prop: Property)(root: propertyable): Unit = {
      val nv = getOrMakeProp(root, prop)
      println(nv.as(classOf[RDFNode]))
      //root.removeAll(prop)
      println(nv.as(classOf[RDFNode]))
      val cont: T = builder()
      cont.add(nv)
      root.addProperty(prop, cont)
      println(cont.as(classOf[RDFNode]))
    }
    def seqm(prop: Property)(root: propertyable): Unit =  {
      var nv = getOrMakeProp(root, prop)
      root.removeAll(prop)
      var cont = m.createSeq()
      root.addProperty(prop, cont.as(classOf[RDFNode]))
      cont.add(nv.as(classOf[RDFNode]) orNull)
    }
    def altm(prop: Property)(root: propertyable): Unit =  {
      var nv = getOrMakeProp(root, prop)
      root.removeAll(prop)
      var cont: Alt = m.createAlt()
      cont.add(nv.as(classOf[RDFNode]) orNull)
      root.addProperty(prop, cont)
    }
    def bagm(prop: Property)(root: propertyable): Unit =  {
      var nv = getOrMakeProp(root, prop)
      println("Bagm: " + nv.as(classOf[RDFNode]))
      root.removeAll(prop)
      var cont = m.createBag()
      root.addProperty(prop, cont.as(classOf[RDFNode]))
      cont.add(nv.as(classOf[RDFNode]) orNull)
    }

    //def altm = resource2container(Unit => m.createAlt()) _
    //def bagm = resource2container(Unit => m.createBag()) _
    //val (seqm, bagm, altm) = Seq((Unit) => m.createSeq(), (Unit) => m.createBag(), (Unit) => m.createAlt()).map(resource2container(_)_) 
    //Manually unroll this, as JVM type erasure means it can't be done with a type parameter
    def asSeq(root: propertyable) = root.as(classOf[JSeq]) orNull
    def asBag(root: propertyable) = root.as(classOf[Bag]) orNull
    def asAlt(root: propertyable) = root.as(classOf[Alt]) orNull
    def container2container(builder: Unit => JContainer)(from: propertyable => JContainer, prop: Property)(root: propertyable): Unit = {
      val te = getOrMakeProp(root, prop)
      println(te.as(classOf[RDFNode]) getOrElse "")
      val nv: Seq[RDFNode] = from(te)
      root.removeAll(prop)
      val cont: JContainer = builder()
      nv.foreach(cont.add(_))
      root.addProperty(prop, cont)
    }
    //val a: Seq[Property => propertyable => Unit] = Seq(Unit => m.createSeq, Unit => m.createBag, Unit => m.createAlt).map(container2container)
    val placeholders = (asSeq _, asBag _, asAlt _)
    def toSeq = container2container(Unit => m.createSeq()) _
    def toBag = container2container(Unit => m.createBag()) _
    def toAlt = container2container(Unit => m.createAlt()) _

    def tryc[T <: RDFNode](r: propertyable, clazz: Class[T], meth: String): Boolean = {
      var ret = false
      try {
        r.as(clazz).foreach((a: RDFNode) => if (clazz.getMethod(meth).invoke(a).asInstanceOf[java.lang.Boolean].booleanValue) ret = true)
      } catch {
        case _ => {}
      }
      ret
    }

    def bagseqaltorres(prop: Property)(root: propertyable): Int = {
      val r = getOrMakeProp(root, prop)
      //Manually unroll this to get around Jena's stupid type problems
      if (tryc(r, classOf[JSeq], "isSeq"))
        return 0
      else if (tryc(r, classOf[Bag], "isBag"))
        return 1
      else if (tryc(r, classOf[Alt], "isAlt"))
        return 2
      return 3
    }
    def vcard(root: propertyable): FlowPanel = CompoundColumnEditor(root, Seq( 
      a => Interconvertable(a, vcfnorn, Seq(Seq(nop _, vcp2vcfn _), Seq(vcfn2vcp _, nop _)),
          Seq(("vcard:N",
          b => CompoundEditor(getOrMakeProp(b, vcn), Seq(
            labeledtext("Given Name: ", vcg), labeledtext("Other Name: ", vco), labeledtext("Family Name: ", vcf)
          ))),
          ("vcard:FN",
            labeledtext("Full Name: ", vcfn)
          ))),
      CompoundEditor(_, Seq(
        c => CompoundEditor(getOrMakeProp(c, vcemail), Seq(labeledtext("Email: ", rdfval), labeleduritext("Type: ", rdftype)), border0 = Swing.TitledBorder(Swing.LineBorder(new Color(3010101).darker.darker.darker), "Email details")),
        c => CompoundEditor(getOrMakeProp(c, vctel), Seq(labeledtext("Number: ", rdfval), labeleduritext("Type: ", rdftype)), border0 = Swing.TitledBorder(Swing.LineBorder(new Color(3010101).darker.darker.darker), "Telephone details"))
        )),
      a => CompoundEditor(getOrMakeProp(a, vcorg), Seq(
        labeledtext("Org Name: ", vcorgn), labeledtext("Org Unit: ", vcorgu)))
      )
    )
    def nop(a: propertyable) = Unit
    def containerwidget(root: propertyable, pred: Property, builder: propertyable => FlowPanel): Interconvertable =
      Interconvertable(root, bagseqaltorres(pred), Seq(
        Seq(nop, toBag(placeholders._2, pred), toAlt(placeholders._3, pred), seq2resource(pred)),
        Seq(toSeq(placeholders._1, pred), nop, toAlt(placeholders._3, pred), bag2resource(pred)),
        Seq(toSeq(placeholders._1, pred), toBag(placeholders._2, pred), nop, alt2resource(pred)),
        Seq(seqm(pred), bagm(pred), altm(pred), nop)),
      Seq(("Seq",
        z => ContEditor[JSeq](root, pred, Unit => m.createSeq(), getOrMakeProp(z, pred).as(classOf[JSeq]) orNull, e => builder(e))),
       ("Bag",
        z => ContEditor[Bag](root, pred, Unit => m.createBag(), getOrMakeProp(z, pred).as(classOf[Bag]) orNull, e => builder(e))),
       ("Alt",
        z => ContEditor[Alt](root, pred, Unit => m.createAlt(), getOrMakeProp(z, pred).as(classOf[Alt]) orNull, e => builder(e))),
       ("Single",
        z => builder(getOrMakeProp(z, pred)))
      ))

    val controls = CompoundColumnEditor(aboutModel, Seq(      
      a => CompoundEditor(a, Seq(b => containerwidget(b, dcc, vcard _)), border0 = Swing.TitledBorder(Swing.LineBorder(new Color(3010101).darker.darker.darker), "Creator")),
      a => CompoundEditor(a, Seq(b => containerwidget(b, dcco, vcard _)), border0 = Swing.TitledBorder(Swing.LineBorder(new Color(3010101).darker.darker.darker), "Contributors")),
      labeledtext("Publisher: ", dcpub),
      labeledtext("Rights: ", dcrights),
      a => CompoundEditor(getOrMakeProp(a, dctcreated), Seq(labeledtext("Date created: ", dctdate))),
      labeledbigtext("Abstract: ", dctabstract)
    ))
    new ScrollPane(new FlowPanel(controls, Button("Save metadata to file") {
      def stripRDF(el: Node): Node = 
        el match {
          case b: Text => (b)
          case <rdf>{inner @ _*}</rdf> => new Text("")
          case <RDF>{inner @ _*}</RDF> => new Text("")
          case a: Elem => (new Elem(a.prefix, a.label, a.attributes, a.scope, a.child.map(stripRDF) : _*))
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
    }))
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
class ColumnPanel extends FlowPanel {
  override lazy val peer: javax.swing.JPanel = 
    new javax.swing.JPanel() with SuperMixin
  peer.setLayout(new BoxLayout(peer, BoxLayout.Y_AXIS))
}

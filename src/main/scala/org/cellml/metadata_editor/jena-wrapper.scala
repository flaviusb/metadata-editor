// This wrapper mitigates some of the brokenness of the intersection of Jena's worldview, the reality of RDF, and Java's laughable type system.

package org.cellml.metadata_editor

import com.hp.hpl.jena.rdf.arp._
import com.hp.hpl.jena.rdf.model.{Model, Alt, Bag, Seq => JSeq, Container => JContainer, RDFNode, ModelFactory, Property, Statement, Resource}


object JenaWrapper {
  trait stringable { def getString(): String }
  trait propertyable extends stringable {
    def getProperty(a: Property): Option[propertyable]
    def addProperty(a: Property, b: String): Unit 
    def addProperty(a: Property, b: RDFNode): Unit
    def hasProperty(a: Property): Boolean
    def removeAll(a: Property): Unit
    def getModel(): Option[Model]
    def canAs[A <: RDFNode](clazz: Class[A]): Boolean
    def as[A <: RDFNode](clazz: Class[A]): Option[A]
  }
  // Wrap both statements and resources into a uniform interface, which maps
  // both Resources and Literals into a trait 'propertyable'.
  implicit def stmt2saferes(stmt: Statement): propertyable = {
    if (stmt.getObject().isLiteral())
      new litResWrapper(stmt.getObject())
    else
      new safeResWrapper(stmt.getResource())
  }
  implicit def res2prop(res: Resource): propertyable = {
    if (res.isLiteral())
      new litResWrapper(res)
    else
      new safeResWrapper(res)
  }
  class litResWrapper(proxy: RDFNode) extends propertyable {
    def getProperty(a: Property): Option[propertyable] = None
    def addProperty(a: Property, b: String) = Unit
    def addProperty(a: Property, b: RDFNode) = Unit
    def hasProperty(a: Property) = false
    def removeAll(a: Property) = Unit
    def getModel(): Option[Model] = None 
    def canAs[A <: RDFNode](clazz: Class[A]): Boolean = proxy.canAs(clazz)
    def as[A <: RDFNode](clazz: Class[A]): Option[A] = {
      if(canAs(clazz)) {
        try {
          Some(proxy.as(clazz))
        } catch {
          case e => None
        }
      }
      else
        None
    }
    def getString(): String = proxy.toString()
  }
  class safeResWrapper(proxy: Resource) extends propertyable {
    def getProperty(a: Property): Option[propertyable] = {
      if (!proxy.hasProperty(a))
        None
      else
        Some(proxy.getProperty(a))
    }
    def addProperty(a: Property, b: String) = proxy.addProperty(a, b)
    def addProperty(a: Property, b: RDFNode) = proxy.addProperty(a, b)
    def hasProperty(a: Property) = proxy.hasProperty(a)
    def removeAll(a: Property) = proxy.removeAll(a)
    def getModel(): Option[Model] = Some(proxy.getModel())
    def canAs[A <: RDFNode](clazz: Class[A]): Boolean = proxy.canAs(clazz)
    def as[A <: RDFNode](clazz: Class[A]): Option[A] = {
      if(canAs(clazz)) {
        try {
          Some(proxy.as(clazz))
        } catch {
          case e => None
        }
      }
      else
        None
    }
    def getString(): String = proxy.toString()
  }
  // It makes more sense to access an RDF container using Scala's rich container
  // stuff than using Jena's crippled interface. We implicitly convert where necessary; at
  // this point the conversion is only useful for non-destructive operations, as
  // we do not do any automatic alteration of the underlying RDF based on alteration to the
  // implicitly generated object. 
  implicit def container2seq(thing: JContainer): Seq[RDFNode] = {
    var f: Seq[RDFNode] = Seq()
    var iter = thing.iterator
    while(iter.hasNext)
      f = f :+ iter.next()
    iter.close()
    f
  }
}

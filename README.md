A simple cellml metadata editor in Scala.
=========================================

This requires [Scala 2.8][scala], and [Jena][jena]. Build with [sbt][sbt], with the commands
    sbt update
    sbt compile

Docs require [Rocco][rocco] to generate, with the command
    sbt literatedocs

The jar can be invoked on the command line, as long as the Scala 2.8 jars, the Jena jars and the metadata-editor jar is on the classpath, with the invocation

    java org.cellml.metadata_editor.MetadataEditor

[scala]: <http://scala-lang.org/> "Scala language"
[jena]:  <http://jena.sourceforge.net/> "Jena RDF Framework"
[sbt]:   <http://code.google.com/p/simple-build-tool/> "Simple build tool"
[rocco]: <http://github.com/rtomayko/rocco> "Rocco documentation generator"

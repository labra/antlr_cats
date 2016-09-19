lazy val antlr_example =
  project.in(file("."))

name := "antlr-test1"

scalaVersion := "2.11.8"

antlr4Settings

antlr4GenListener in Antlr4 := true

antlr4GenVisitor in Antlr4 := true

antlr4Dependency in Antlr4 := "org.antlr" % "antlr4" % "4.5"

antlr4PackageName in Antlr4 := Some("es.weso.shex.parser")

libraryDependencies ++= Seq(
 compilerPlugin("org.spire-math" %% "kind-projector"   % "0.8.0"),
 compilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full),
 "es.weso" % "srdf-jvm_2.11" % "0.0.8",
 "org.typelevel" %% "cats" % "0.7.2"
)

resolvers += Resolver.bintrayRepo("labra", "maven")

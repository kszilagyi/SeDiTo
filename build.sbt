
import sbt.Project.projectToRef
import sbt.Keys._
import sbt.librarymanagement

name := "SeDiTo"

version := "0.1"

scalaVersion := "2.12.6"


//Skeleton copied and modified from: https://github.com/ochrons/scalajs-spa-tutorial

/** Options for the scala compiler */
val customScalacOptions = Seq(
  "-Xlint",
  "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
  "-encoding", "utf-8",                // Specify character encoding used by source files.
  "-explaintypes",                     // Explain type errors in more detail.
  "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
  "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
  "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
  "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
  "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
  "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
  "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
  "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
  "-Xlint:option-implicit",            // Option.apply used implicit view.
  "-Xlint:package-object-classes",     // Class or object defined in package object.
  "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
  "-Xlint:unsound-match",              // Pattern match may not be typesafe.
  "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  "-Ywarn-dead-code",                  // Warn when dead code is identified.
  "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
  "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
  "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
  "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
  "-Ywarn-numeric-widen",              // Warn when numerics are widened.
  "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals",              // Warn if a local definition is unused.
  "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates",            // Warn if a private member is unused.
  "-Ywarn-unused:params",
  "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.

)


def commonSettings = Seq(
  wartremoverErrors ++= Warts.allBut(Wart.Any, Wart.Nothing, Wart.ImplicitParameter, Wart.Product, Wart.Serializable,
    Wart.PublicInference, Wart.ImplicitConversion, Wart.Option2Iterable),

  sources in (Compile, doc) := Seq.empty,
  publishArtifact in (Compile, packageDoc) := false,
  scalacOptions ++= customScalacOptions,

  resolvers += librarymanagement.Resolver.mavenLocal

)

lazy val common = (project in file("common"))
  .settings(
    name := "common",
    libraryDependencies += "org.log4s" %% "log4s" % "1.4.0",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3",
    libraryDependencies += "io.spray" %%  "spray-json" % "1.3.4",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test",
    libraryDependencies += "com.beachape" %% "enumeratum" % "1.5.12",
    libraryDependencies += "org.bitbucket.cowwoc" % "diff-match-patch" % "1.1",
    libraryDependencies += "info.debatty" % "java-string-similarity" % "1.1.0",
    libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1",
    libraryDependencies += "org.apache.commons" % "commons-text" % "1.3",
    libraryDependencies += "org.scala-lang.modules" % "scala-java8-compat_2.12" % "0.9.0",
    commonSettings
  )

lazy val gui = (project in file("gui"))
  .settings(
    name := "gui",
    libraryDependencies += "org.fxmisc.richtext" % "richtextfx" % "0.9.1",
    commonSettings
  ).dependsOn(common % "compile->compile;test->test")
   .dependsOn(aligner % "compile->compile;test->test")


lazy val aligner = (project in file("aligner"))
  .settings(
    name := "aligner",
    commonSettings,
    libraryDependencies += "com.github.haifengl" %% "smile-scala" % "1.5.1"
  ).dependsOn(common % "compile->compile;test->test")

lazy val root = (project in file("."))
  .aggregate(gui, common, aligner)


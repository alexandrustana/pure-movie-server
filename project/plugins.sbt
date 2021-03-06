//=============================================================================
//=============================================================================

/**
  * The best thing since sliced bread.
  *
  * https://github.com/scalameta/scalafmt
  */
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.3.4") //https://github.com/scalameta/sbt-scalafmt/releases

//=============================================================================
//=============================================================================

/**
  * Refactoring/linting tool for scala.
  * Enable on per-use basis because it currently breaks a lot of IDEs
  *
  * https://github.com/scalacenter/scalafix
  * https://scalacenter.github.io/scalafix/
  *
  * From docs:
  * {{{
  *   // ===> sbt shell
  *
  *   > scalafixEnable                         // Setup scalafix for active session.
  *
  *   > scalafix                               // Run all rules configured in .scalafix.conf
  *
  *   > scalafix RemoveUnused                  // Run only RemoveUnused rule
  *
  *   > myProject/scalafix RemoveUnused // Run rule in one project only
  *
  * }}}
  */
//addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.5")

//=============================================================================
//=============================================================================

/**
  * Used to create one big fat jar which contains all dependencies of this application
  *
  * https://github.com/sbt/sbt-assembly
  */
addSbtPlugin("com.eed3si9n" %% "sbt-assembly" % "0.14.10") //https://github.com/sbt/sbt-assembly

//=============================================================================
//=============================================================================

/**
  * neat way of visualizing the dependency graph both in the sbt repl, and to export
  * it as an .svg
  *
  * https://github.com/jrudolph/sbt-dependency-graph
  *
  */
//addSbtPlugin("net.virtual-void" %% "sbt-dependency-graph" % "0.9.2")

//=============================================================================
//=============================================================================

/**
  * Used to build the documentation.
  *
  * https://github.com/47deg/sbt-microsites
  */
//addSbtPlugin("com.47deg" % "sbt-microsites" % "0.7.18")

//=============================================================================
//=============================================================================

/**
  *
  * Used by sbt-microsites
  *
  * https://github.com/sbt/sbt-ghpages
  */
//addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.6.2")

//=============================================================================
//=============================================================================

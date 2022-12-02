val zioVersion = "2.0.4"
val organizeImportsVersion = "0.6.0"

inThisBuild(
  List(
    name := "advent-of-code-2022",
    normalizedName := "aoc2022",
    organization := "io.github.polentino",
    scalaVersion := "3.2.1",
    version := "0.0.1",
    homepage := Some(url("https://github.com/polentino/advent-of-code-2022")),
    licenses := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "polentino",
        "Diego Casella",
        "polentino911@gmail.com",
        url("https://www.github.com/polentino")
      )
    ),
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalafixDependencies += "com.github.liancheng" %% "organize-imports" % organizeImportsVersion
  )
)


Global / onChangedBuildSource := ReloadOnSourceChanges
testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-test" % zioVersion,
  "dev.zio" %% "zio-streams" % zioVersion % "test",
  "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
)

addCommandAlias("fmt", "scalafmtAll; scalafmtAll; scalafixAll; scalafixAll")
addCommandAlias("check", "all scalafmtCheck test:scalafmtCheck")

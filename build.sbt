import sys.process.*

val scala3Version = "3.4.1"

val compilerOptions = Seq(
    "-encoding",
    "utf8",
    "-feature",
    "-unchecked",
    "-deprecation",
    "-language:implicitConversions",
    "-Wvalue-discard",
    "-Wunused:all"
    // "-Xfatal-warnings"
    // "-explain"
    // "-Vprofile",
)

val kyoVersion = "0.9.1"

scalaVersion                       := scala3Version
ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
sonatypeProfileName                := "io.github.johnhungerford"
sonatypeRepository                 := "https://s01.oss.sonatype.org/service/local"

publish / skip                     := true

lazy val `zikyo-settings` = Seq(
    fork               := true,
    scalaVersion       := scala3Version,
    crossScalaVersions := List(scala3Version),
    scalacOptions ++= compilerOptions,
    scalafmtOnCompile := true,
    organization := "io.github.johnhungerford",
    organizationName := "johnhungerford",
    homepage          := Some(url("https://getkyo.io")),
    licenses          := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
        Developer(
            "johnhungerford",
            "John Hungerford",
            "jiveshungerford@gmail.com",
            url("https://github.com/johnhungerford/")
        )
    ),
    ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org",
    sonatypeRepository                 := "https://s01.oss.sonatype.org/service/local",
    sonatypeProfileName                := "io.getkyo",
    Test / testOptions += Tests.Argument("-oDG"),
    ThisBuild / versionScheme := Some("early-semver"),
    scalacOptions ++= Seq("-release:11"),
    Test / javaOptions += "--add-opens=java.base/java.lang=ALL-UNNAMED",
    libraryDependencies += "io.getkyo" %%% "kyo-core" % kyoVersion,
)

lazy val skipPublish = Seq(
    publishArtifact                        := false,
    publish / skip                         := true,
    Compile / packageBin / publishArtifact := false,
    Compile / packageDoc / publishArtifact := false,
    Compile / packageSrc / publishArtifact := false,
)

lazy val zikyo =
    crossProject(JVMPlatform)
        .in(file("."))
        .settings(
            name                                   := "zikyo",
            organization                           := "io.github.johnhungerford",
            scalaVersion                           := scala3Version,
            skipPublish,
            `zikyo-settings`
        ).aggregate(
            `zikyo-core`,
            `zikyo-examples`,
        )

lazy val `zikyo-core` =
    crossProject(JSPlatform, JVMPlatform)
        .withoutSuffixFor(JVMPlatform)
        .crossType(CrossType.Full)
        .in(file("zikyo-core"))
        .settings(
            `zikyo-settings`,
			libraryDependencies += "org.scalatest" %%% "scalatest"       % "3.2.16"     % Test,
        )
        .jsSettings(`js-settings`)


lazy val `zikyo-examples` =
    crossProject(JVMPlatform)
        .withoutSuffixFor(JVMPlatform)
        .crossType(CrossType.Full)
        .dependsOn(`zikyo-core`)
        .in(file("zikyo-examples"))
        .settings(
            Compile / doc / sources                              := Seq.empty,
            libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-json-zio" % "1.10.0",
            libraryDependencies += "io.getkyo" %% "kyo-direct" % kyoVersion,
            libraryDependencies += "io.getkyo" %% "kyo-os-lib" % kyoVersion,
            libraryDependencies += "io.getkyo" %% "kyo-tapir" % kyoVersion,
            skipPublish,
            `zikyo-settings`,
        )

import org.scalajs.jsenv.nodejs.*

lazy val `js-settings` = Seq(
    Compile / doc / sources := Seq.empty,
    fork                    := false,
    jsEnv := new NodeJSEnv(NodeJSEnv.Config().withArgs(List("--max_old_space_size=5120")))
)

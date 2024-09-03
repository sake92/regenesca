import $ivy.`io.chris-kipp::mill-ci-release::0.1.10`

import mill._
import mill.scalalib._
import mill.scalalib.publish._
import io.kipp.mill.ci.release.CiReleaseModule

object regenesca extends ScalaModule with CiReleaseModule {

  def artifactName = "regenesca"

  def pomSettings = PomSettings(
    organization = "ba.sake",
    url = "https://github.com/sake92/regenesca",
    licenses = Seq(License.Common.Apache2),
    versionControl = VersionControl.github("sake92", "regenesca"),
    description =
      "Regenesca library - Refactoring Generator of Source Code for Scala",
    developers = Seq(
      Developer("sake92", "Sakib Hadžiavdić", "https://sake.ba")
    )
  )

  def scalaVersion = "2.13.14"

  def ivyDeps = Agg(
    ivy"org.scalameta::scalameta:4.9.9",
    ivy"org.scala-lang.modules::scala-collection-contrib:0.3.0",
    ivy"com.lihaoyi::pprint:0.9.0"
  )

  object test extends ScalaTests with TestModule.Munit {
    def ivyDeps = Agg(
      ivy"org.scalameta::munit::1.0.1"
    )
  }
}

object example extends ScalaModule {
  def scalaVersion = "2.13.14"
  def moduleDeps = Seq(regenesca)
}

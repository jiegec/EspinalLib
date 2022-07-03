import mill._
import mill.scalalib.publish._
import scalalib._
import scalafmt._
import coursier.maven.MavenRepository
import $ivy.`com.goyeau::mill-scalafix_mill0.10:0.2.8`
import com.goyeau.mill.scalafix.ScalafixModule

// learned from https://github.com/OpenXiangShan/fudian/blob/main/build.sc
val defaultVersions = Map(
  "scalatest" -> ("org.scalatest", "3.2.10", false),
  "spinalhdl-core" -> ("com.github.spinalhdl", "1.6.1", false),
  "spinalhdl-lib" -> ("com.github.spinalhdl", "1.6.1", false),
  "spinalhdl-idsl-plugin" -> ("com.github.spinalhdl", "1.6.1", false),
  "spinalhdl-crypto" -> ("com.github.spinalhdl", "1.1.1", false)
)

def getVersion(dep: String) = {
  val (org, ver, cross) = defaultVersions(dep)
  val version = sys.env.getOrElse(dep + "Version", ver)
  if (cross)
    ivy"$org:::$dep:$version"
  else
    ivy"$org::$dep:$version"
}

trait CommonModule extends ScalaModule {
  def scalaVersion = "2.12.14"
}

object espinallib
    extends CommonModule
    with ScalafmtModule
    with ScalafixModule
    with PublishModule {
  override def ivyDeps = super.ivyDeps() ++ Agg(
    getVersion("spinalhdl-core"),
    getVersion("spinalhdl-lib"),
    getVersion("spinalhdl-crypto")
  )

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    getVersion("spinalhdl-idsl-plugin")
  )

  // for scalafix rules
  override def scalacOptions =
    Seq("-Ywarn-unused", "-Ywarn-adapted-args", "-deprecation")
  override def scalafixIvyDeps = Agg(
    ivy"com.github.liancheng::organize-imports:0.5.0"
  )

  object test extends Tests with TestModule.ScalaTest {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      getVersion("scalatest")
    )
  }

  // publish
  def publishVersion = "1.0-SNAPSHOT"
  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "je.jia",
    url = "https://github.com/jiegec/espinallib",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("jiegec", "espinallib"),
    developers = Seq()
  )
}

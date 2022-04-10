package helper

import main.StringDecryption.ErrorLogger
import org.opalj.br.analyses.Project
import org.opalj.log.OPALLogger

import java.io.{File, InputStream}
import java.net.URL
import java.util.zip.ZipFile
import scala.sys.process.Process

object APKManager {
  private val userDir: String = System.getProperty("user.dir")
  private val jarDirectory: String = s"$userDir/jars"
  private val jarDirectoryFile = new File(jarDirectory)
  private def jarDirectoryExists = jarDirectoryFile.exists()

  if (!jarDirectoryExists) {
    jarDirectoryFile.mkdir()
  }
}

class APKManager(pathToAPK: String) {
  import helper.APKManager.jarDirectory

  private val apkName = pathToAPK.split("/").last
  val pathToJAR: String = s"$jarDirectory/$apkName.jar"
  private val jarFile = new File(pathToJAR)

  private val stdLib: File = org.opalj.bytecode.RTJar

  repackageJarIfNeeded()

  private val androidLib = new File(AndroidJarAnalysis.identifyAndroidJar(jarFile))
  val opalProject : Project[URL] = Project(Array(jarFile), Array(stdLib, androidLib))

  OPALLogger.updateLogger(opalProject.logContext, ErrorLogger)

  private val apkZipFile = new ZipFile(pathToAPK)

  def getAssetStream(pathToAsset: String) : InputStream = {
    class AssetStreamAccessException extends RuntimeException
    try {
       apkZipFile.getInputStream(apkZipFile.getEntry(s"assets/$pathToAsset"))
    } catch {
      case _ : Throwable => throw new AssetStreamAccessException()
    }
  }

  private def repackageJarIfNeeded() : Unit = {
    if (!jarFile.exists()) {
      println(s"Repackaging $apkName into JAR...")
      val repackagingResult = Process(
        Seq(
          "bash",
          "-c",
          s"cd src/main/python/enjarify-master && pypy3 -O -m enjarify.main $pathToAPK -o $pathToJAR -f"
        )
      ).!!

      if (!repackagingResult.contains("translated successfully")) {
        class RepackagingUnsuccessfulException extends RuntimeException
        throw new RepackagingUnsuccessfulException()
      }
    } else {
      println(s"Reusing repackaged $apkName.jar for analysis...")
    }
  }



}

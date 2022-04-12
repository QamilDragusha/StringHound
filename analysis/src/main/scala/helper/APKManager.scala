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
  import helper.APKManager.{jarDirectory, userDir}

  private val apkName = pathToAPK.split("/").last

  val pathToJAR: String = s"$jarDirectory/$apkName.jar"
  val pathToResultsDirectory: String = s"$userDir/results/$apkName/"
  val pathToDataDir: String = s"$pathToResultsDirectory/dataDir/"
  val pathToCacheDir: String = s"$pathToResultsDirectory/cacheDir"

  private val jarFile = getJarFile

  lazy val resultsDirectory : File = getResultsDirectory
  lazy val dataDirectory : File = getAppSpecificDataDirectory
  lazy val cacheDirectory : File = getAppSpecificCacheDirectory

  val leaker : Leaker = new Leaker(this)

  private val stdLib: File = org.opalj.bytecode.RTJar

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

  private def getJarFile : File = {
    val jarFile = new File(pathToJAR)
    createRepackagedJarIfNeeded(jarFile)
    jarFile
  }


  private def getResultsDirectory : File = {
    createFileWithDirectory(pathToResultsDirectory)
  }

  private def getAppSpecificDataDirectory : File = {
    createFileWithDirectory(pathToDataDir)
  }

  private def getAppSpecificCacheDirectory : File = {
    createFileWithDirectory(pathToCacheDir)
  }

  private def createFileWithDirectory(path: String) : File = {
    val file = new File(path)
    if (!file.exists()) {
      file.mkdir()
    }
    file
  }

  private def createRepackagedJarIfNeeded(jarFile: File) : Unit = {
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

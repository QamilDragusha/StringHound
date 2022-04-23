package helper

import helper.APKManager.decodedApkDirectoryPath
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

  private val decodedApkDirectoryPath: String = s"$userDir/decodedApks"
  private val decodedApkDirectory = new File(decodedApkDirectoryPath)
  private def decodedApkDirectoryExists = decodedApkDirectory.exists()

  if (!jarDirectoryExists) {
    jarDirectoryFile.mkdir()
  }

  if (!decodedApkDirectoryExists) {
    decodedApkDirectory.mkdir()
  }
}

class APKManager(pathToAPK: String) {
  import helper.APKManager.{jarDirectory, userDir}

  private val apkName = pathToAPK.split("/").last

  val pathToJAR: String = s"$jarDirectory/$apkName.jar"
  val pathToResultsDirectory: String = s"$userDir/results/$apkName/"
  val pathToDataDir: String = s"$pathToResultsDirectory/dataDir/"
  val pathToCacheDir: String = s"$pathToResultsDirectory/cacheDir"

  val pathToDecodedAPKDirectory : String = s"$decodedApkDirectoryPath/$apkName"

  private val jarFile = getJarFile

  private val decodedAPKDirectory : File = getDecodedAPKDirectory
  lazy val resultsDirectory : File = getResultsDirectory
  lazy val dataDirectory : File = getAppSpecificDataDirectory
  lazy val cacheDirectory : File = getAppSpecificCacheDirectory

  lazy val androidManifestReader = new AndroidManifestReader(decodedAPKDirectory)


  val leaker : Leaker = new Leaker(this)

  private val stdLib: File = org.opalj.bytecode.RTJar

  private val androidLib = new File(AndroidJarAnalysis.identifyAndroidJar(jarFile))
  val opalProject : Project[URL] = Project(Array(jarFile), Array(stdLib, androidLib))

  OPALLogger.updateLogger(opalProject.logContext, ErrorLogger)

  private val apkZipFile = new ZipFile(pathToAPK)

  // TODO qamil: Das wäre jetzt auch außerhalb der APK möglich?
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
    createRepackagedJarIfAbsent(jarFile)
    jarFile
  }

  private def getDecodedAPKDirectory : File = {
    val decodedAPKDirectory = new File(pathToDecodedAPKDirectory)
    decodeAPKIfAbsent(decodedAPKDirectory)
    decodedAPKDirectory
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

  private def createRepackagedJarIfAbsent(jarFile: File) : Unit = {
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

  private def decodeAPKIfAbsent(decodedApkDirectory: File) : Unit = {
    if(!decodedApkDirectory.exists()) {
      println(s"Decoding $apkName...")
      decodeAPK()
    } else {
      println(s"Resusing decoding of $apkName for analysis...")
    }
  }

  private def decodeAPK() : Unit = {
   Process(
     Seq(
       "bash",
       "-c",
       s"apktool d $pathToAPK -o $pathToDecodedAPKDirectory"
     )
   ).!!
  }



}

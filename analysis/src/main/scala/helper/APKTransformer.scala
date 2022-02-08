package helper

import scala.sys.process.Process

case class RepackagingResult(pathToJar: String, repackagingSuccessful: Boolean)


object APKTransformer {

  private val userDir : String = System.getProperty("user.dir")
  private val outputPath = userDir + "/output.jar"

  private def printRepackagingInitializationReport(pathToApk: String) : Unit = {
    val apkName = pathToApk.split("/").last
    println(s"Repackaging $apkName...")
  }

  private def printRepackagingCompletionReport(repackagingSuccessful: Boolean) : Unit = {
    if (repackagingSuccessful) {
      println("Repackaging successful")
    } else {
      println("Repackaging the APK into a JAR failed")
    }
  }

  private def computeRepackagingProcessResult(processResult: String) : RepackagingResult = {
    val repackagingSuccessful : Boolean = processResult.contains("translated successfully")
    printRepackagingCompletionReport(repackagingSuccessful)
    RepackagingResult(outputPath, repackagingSuccessful)
  }

  private def computeRepackagingProcessResult(processResult: String, outputPath: String) : RepackagingResult = {
    val repackagingSuccessful : Boolean = processResult.contains("translated successfully")
    printRepackagingCompletionReport(repackagingSuccessful)
    RepackagingResult(outputPath, repackagingSuccessful)
  }

  def repackageAPKIntoJar(pathToApk : String) : RepackagingResult = {
    printRepackagingInitializationReport(pathToApk)
    val result = Process(Seq("bash", "-c", s"cd src/main/python/enjarify-master && pypy3 -O -m enjarify.main $pathToApk -o $outputPath -f")).!!
    computeRepackagingProcessResult(result)
  }

  def repackageAPKIntoJar(pathToApk : String, outputDirectory : String) : RepackagingResult = {
    printRepackagingInitializationReport(pathToApk)
    val appName = pathToApk.replaceAll(".apk", "")
    val result = Process(Seq("bash", "-c", s"cd src/main/python/enjarify-master && pypy3 -O -m enjarify.main $pathToApk -o $outputDirectory/$appName.jar -f")).!!
    computeRepackagingProcessResult(result, s"$outputDirectory/$appName.jar")
  }

}


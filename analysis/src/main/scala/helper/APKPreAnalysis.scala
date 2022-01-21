package helper

import java.util.zip.{ZipEntry, ZipFile}
import java.io._


/**
  * Quickly analyzes a given APK whether it seems to be loading classes at
  * runtime
  *
  * @param pathToApk The full file path to the APK to be preanalyzed
  */
@throws(classOf[java.util.zip.ZipException])
class ApkPreAnalysis(val pathToApk : String) {

  private val apkFile : File = new File(pathToApk)
  private val apkToAnalyzeAsZip : ZipFile = new ZipFile(apkFile)

  def analyze() : APKPreAnalysisResult = {

    val result : APKPreAnalysisResult = new APKPreAnalysisResult()

    val apkEntries = apkToAnalyzeAsZip.entries()

    while (apkEntries.hasMoreElements) {
      val apkEntry = apkEntries.nextElement()
      if (isDexFile(apkEntry)) {
        result += analyzeDexFile(apkEntry)
      }
    }

    result

  }

  private def analyzeDexFile(zipEntry : ZipEntry) : APKPreAnalysisResult = {

    val analysisResult = new APKPreAnalysisResult()

    val dexFileInputStream : InputStream = apkToAnalyzeAsZip.getInputStream(zipEntry)
    val dexFileContents = inputStreamToString(dexFileInputStream)
    if (dexFileContents.contains("ClassLoader")) {
      analysisResult.observedPotentialClassLoaderUsage = true
    }
    if (dexFileContents.contains("reflect")) {
      analysisResult.observedPotentialReflectionCallUsage = true
    }

    analysisResult
  }

  private def isDexFile(zipEntry: ZipEntry) : Boolean = {
    val fileName = zipEntry.getName
    fileName.contains(".dex")
  }

  // Taken from https://newbedev.com/idiomatic-way-to-convert-an-inputstream-to-a-string-in-scala
  private def inputStreamToString(is: InputStream) : String= {
    val inputStreamReader = new InputStreamReader(is)
    val bufferedReader = new BufferedReader(inputStreamReader)
    Iterator continually bufferedReader.readLine takeWhile (_ != null) mkString
  }

}

class APKPreAnalysisResult(var observedPotentialClassLoaderUsage: Boolean = true, var observedPotentialReflectionCallUsage: Boolean = true) {

  val observedBehaviourOfInterest : Boolean = observedPotentialClassLoaderUsage || observedPotentialReflectionCallUsage

  def accumulateResults(result: APKPreAnalysisResult) : Unit = {
    observedPotentialClassLoaderUsage = observedPotentialClassLoaderUsage || result.observedPotentialClassLoaderUsage
    observedPotentialReflectionCallUsage = observedPotentialReflectionCallUsage || result.observedPotentialReflectionCallUsage
  }

  var resultsReport : String = {
    if (observedBehaviourOfInterest) {
      "The Pre-Analysis of the APK DID observe behaviour of interest."
    } else {
       "The Pre-Analysis of the APK did not reveal behaviour of interest."
    }
  }

  def +=(other: APKPreAnalysisResult) : Unit = accumulateResults(other)

}


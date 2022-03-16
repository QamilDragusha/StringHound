package main

import helper.{APKPreAnalysisResult, APKTransformer, ApkPreAnalysis, ClassLoaderFinder, RepackagingResult}
import org.apache.commons.cli.{CommandLine, DefaultParser, Options}

object ClassDecryption {

  val options = new Options()

  options.addOption("f", "file", true, "input file <apk or jar>")
  options.addOption("o", "output", true, "output directory for JARs")

  def main(args: Array[String]): Unit = {

    val command: DefaultParser = new DefaultParser()
    val line: CommandLine = command.parse(options, args)

    val fileName = line.getOptionValue('f')
    val outputDir : Option[String] = if (line.hasOption('o')) Option(line.getOptionValue('o')) else None

    if (fileName.endsWith(".apk")) {
     analyzeApk(fileName, outputDir)
    } else if (fileName.endsWith(".jar")) {
      println("Analyzing given jar...")
//      ClassLoaderFinder.findCLassLoaderReferenceMethods(fileName)
    }

  }

  def analyzeApk(filePath: String, outputDir : Option[String]) : Unit = {
    val apkPreAnalysisResult = new ApkPreAnalysis(filePath).analyze()
    println(apkPreAnalysisResult.resultsReport)
    if ( apkPreAnalysisResult != null &&  apkPreAnalysisResult.observedBehaviourOfInterest ) {
      println("Proceeding with the more intensive analysis of the JAR equivalent...")
      var result : RepackagingResult = null
      if (outputDir != None) {
         result = APKTransformer.repackageAPKIntoJar(filePath, outputDir.get)
      } else {
         result = APKTransformer.repackageAPKIntoJar(filePath)
      }

      if (result.repackagingSuccessful) {
        // ClassLoaderFinder.findCLassLoaderReferenceMethods(result.pathToJar)
      }
    }
  }




}

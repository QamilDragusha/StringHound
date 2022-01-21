package main

import helper.{APKTransformer, ApkPreAnalysis, ClassLoaderFinder, RepackagingResult}
import org.apache.commons.cli.{CommandLine, DefaultParser, Options}

object ClassDecryption {

  val options = new Options()

  options.addOption("f", "file", true, "input file <apk>")

  def main(args: Array[String]): Unit = {

    val command: DefaultParser = new DefaultParser()
    val line: CommandLine = command.parse(options, args)

    val fileName = line.getOptionValue('f')

    val apkPreAnalysisResult = new ApkPreAnalysis(fileName).analyze()
    println(apkPreAnalysisResult.resultsReport)

    if (apkPreAnalysisResult.observedBehaviourOfInterest) {
      println("Proceeding with the more intensive analysis of the JAR equivalent...")
      val result : RepackagingResult = APKTransformer.repackageAPKIntoJar(fileName)
      if (result.repackagingSuccessful) {
        ClassLoaderFinder.analyzeConcreteJAR(result.pathToJar)
      }
    }


  }




}

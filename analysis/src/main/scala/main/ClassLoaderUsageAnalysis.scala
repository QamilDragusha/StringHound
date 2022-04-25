package main

import helper.{APKManager, AndroidJarAnalysis, ClassLoaderFinder}
import main.AnalysisMode.{AnalysisMode, AnalyzeFromAPK, AnalyzeFromJAR}
import main.StringDecryption.{ErrorLogger, stdLib}
import org.apache.commons.cli.{DefaultParser, Options}
import org.opalj.br.analyses.Project
import org.opalj.log.OPALLogger

import java.io.{File, FileFilter, FileWriter}


/*
  Analyzes apps of a given directory on whether they are using ClassLoaders
 */
object ClassLoaderUsageAnalysis {

  private val outputSteam = new FileWriter(getOutputFile, false)

  def main(args: Array[String]) : Unit = {
    val (path, analysisMode) = parsePathAndAnalysisModeFromArgs(args)
    val analyzeMultipleFiles = multipleFilesToAnalyze(path, analysisMode)

    val pathFile = new File(path)

    if (!pathFile.exists()) {
      println(s"$path not found...")
      outputSteam.close()
      return
    }

    println(s"Analyzing from $path...")

    if (analyzeMultipleFiles) {
      println("Multiple Files to analyze...")
      val filesToAnalyze = pathFile.listFiles(getOnlyFilesToAnalyzeFilter(analysisMode))
      for (file <- filesToAnalyze) {
        analyzeAppFromFile(file, analysisMode)
      }
    } else {
      analyzeAppFromFile(pathFile, analysisMode)
    }

    outputSteam.close()
  }

  def parsePathAndAnalysisModeFromArgs(args: Array[String]) : (String, AnalysisMode) = {

    val jarOption = "jar"
    val apkOption = "apk"
    val pathOption = "p"

    val options = new Options()
    options.addOption(jarOption, false, "Analyze from given jar(s). It is required to have either 'jar' or 'apk' selected")
    options.addOption(apkOption, false, "Analyze from given apk(s). It is required to have either 'jar' or 'apk' selected")
    options.addRequiredOption(pathOption, "path", true, "The path to the given directory / file")

    val commandLineParser = new DefaultParser()
    val commandLine = commandLineParser.parse(options, args)

    val path : String = commandLine.getOptionValue(pathOption)
    assert(commandLine.hasOption(jarOption) || commandLine.hasOption(apkOption))

    if (commandLine.hasOption(jarOption)) {
      (path, AnalysisMode.AnalyzeFromJAR)
    } else {
      (path, AnalysisMode.AnalyzeFromAPK)
    }

  }

  def multipleFilesToAnalyze(path: String, analysisMode: AnalysisMode) : Boolean = {
    analysisMode match {
      case AnalyzeFromJAR => !path.endsWith(".jar")
      case AnalyzeFromAPK => !path.endsWith(".apk")
    }
  }

  def analyzeAppFromFile(appFile: File, analysisMode: AnalysisMode) : Unit = {
    analysisMode match {
      case AnalyzeFromJAR => analyzeJAR(appFile)
      case AnalyzeFromAPK => analyzeAPK(appFile)
    }
  }

  def analyzeJAR(jarFile: File) : Unit = {
    val jarName = jarFile.getAbsolutePath.split("/").last

    val androidLib = new File(AndroidJarAnalysis.identifyAndroidJar(jarFile))
    val project = Project(Array(jarFile), Array(stdLib, androidLib))


    val (classLoaderInstantiations, instantiatedClassLoaderTypes) = new ClassLoaderFinder(project).findClassLoaderInstantiationsAndVariety()

    if (classLoaderInstantiations.nonEmpty) {
      logClassLoaderInstantiationResult(jarName, classLoaderInstantiations.size, instantiatedClassLoaderTypes.size)
    }
  }

  def analyzeAPK(apkFile: File) : Unit = {
    println("Analyzing APK...")
    val apkManager = new APKManager(apkFile.getPath)
    val jarFile = new File(apkManager.pathToJAR)
    analyzeJAR(jarFile)
  }

  def logClassLoaderInstantiationResult(appFileName: String, instantiationAmount: Int, instantiatedClassLoaderTypeAmount : Int) : Unit = {
    outputSteam.write(s"$appFileName: \t \t $instantiationAmount instantiations of $instantiatedClassLoaderTypeAmount different types\n")
  }


  def getOnlyFilesToAnalyzeFilter(analysisMode: AnalysisMode) : FileFilter = {
    (file: File) => {
      val path = file.getPath
      analysisMode match {
        case AnalyzeFromJAR => path.endsWith(".jar")
        case AnalyzeFromAPK => path.endsWith(".apk")
      }
    }
  }



  def getOutputFile: File = {
    val pathToOutputDir = StringDecryption.outputDir + "/classLoaderAnalysis/"
    val outputDir = new File(pathToOutputDir)
    if (!outputDir.exists()) outputDir.mkdir()
    val output = new File(pathToOutputDir + "results.txt")
    if (!output.exists()) output.createNewFile()
    output
  }

}

 object AnalysisMode extends Enumeration {
  type AnalysisMode = Value
  val AnalyzeFromJAR, AnalyzeFromAPK = Value

}

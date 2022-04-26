package tools

import helper.{APKManager, AndroidJarAnalysis, ClassLoaderFinder}
import main.StringDecryption
import main.StringDecryption.{ErrorLogger, outputDir, stdLib}
import org.apache.commons.cli.{DefaultParser, Options}
import org.opalj.br.analyses.Project
import org.opalj.log.{GlobalLogContext, OPALLogger}
import tools.AnalysisMode.{AnalysisMode, AnalyzeFromAPK, AnalyzeFromAny, AnalyzeFromJAR}

import java.io.{BufferedReader, File, FileFilter, FileReader, FileWriter}
import java.net.URL
import scala.collection.mutable.ArrayBuffer

/**  Analyzes apps of a given directory on whether they are using ClassLoaders.
  *
  *  A more detailled usage guide can be found in the tools.md file
  */
object ClassLoaderUsageAnalysis {

  private var outputSteam: FileWriter = _

  def main(args: Array[String]): Unit = {
    OPALLogger.updateLogger(GlobalLogContext, ErrorLogger)
    val (path, analysisMode) = parseAndSetUserDefinedArgs(args)

    println(s"Analyzing from $path...")

    val filesToAnalyze = determineFilesToAnalyze(path, analysisMode).par
    filesToAnalyze.foreach(analyzeAppFromFile(_, analysisMode))

    println("Terminating analysis...")

    outputSteam.close()
  }


  /** Parses the given arguments and returns a Tuple of (the path to the file/directory to analyze, the desired analysis mode)
    *  with which the analysis should be performed with. Also sets the output stream to a custom output path if specified.
    */
  def parseAndSetUserDefinedArgs(
      args: Array[String]
  ): (String, AnalysisMode) = {

    val jarOption = "jar"
    val apkOption = "apk"
    val pathOption = "p"
    val outputOption = "o"

    val options = new Options()
    options.addOption(
      jarOption,
      false,
      "Analyze from given jar(s). Will ignore any APKs if selected"
    )
    options.addOption(
      apkOption,
      false,
      "Analyze from given apk(s). Will ignore any JARs if selected"
    )
    options.addOption(
      outputOption,
      "output",
      true,
      "An optional custom output file/directory for the result file"
    )
    options.addRequiredOption(
      pathOption,
      "path",
      true,
      "The path to the given directory / file or a .txt file containing file paths"
    )

    val commandLineParser = new DefaultParser()
    val commandLine = commandLineParser.parse(options, args)

    val path: String = commandLine.getOptionValue(pathOption)

    val customOutputPath: Option[String] =
      if (commandLine.hasOption(outputOption))
        Some(commandLine.getOptionValue(outputOption))
      else None

    setOutputStream(customOutputPath)

    assert(
      !commandLine.hasOption(jarOption) || !commandLine.hasOption(apkOption),
      "Setting both -jar and -apk ignores every file"
    )

    if (commandLine.hasOption(jarOption)) {
      (path, AnalysisMode.AnalyzeFromJAR)
    } else if (commandLine.hasOption(apkOption)) {
      (path, AnalysisMode.AnalyzeFromAPK)
    } else {
      (path, AnalysisMode.AnalyzeFromAny)
    }

  }

  def determineFilesToAnalyze(
                               path: String,
                               analysisMode: AnalysisMode
                             ): Array[File] = {
    val pathRefersToSingleFile = path.endsWith(".jar") || path.endsWith(".apk")
    val pathRefersToFileContainingPaths = path.endsWith(".txt")

    val pathFile = new File(path)
    if (!pathFile.exists()) {
      println(s"Unable to find $pathFile")
      return Array()
    }

    if (pathRefersToSingleFile) {
      println(s"Analyzing single file $path...")
      Array(pathFile)
    } else if (pathRefersToFileContainingPaths) {
      try {
        val files: ArrayBuffer[File] = ArrayBuffer()

        val bufferedReader = new BufferedReader(new FileReader(pathFile))
        var filePathLine: String = bufferedReader.readLine()

        while (filePathLine != null) {
          if (filePathMatchesAnalysisMode(filePathLine, analysisMode)) {
            val file = new File(filePathLine)
            if (file.exists()) files.append(file)
          }
          filePathLine = bufferedReader.readLine()
        }

        val fileCount = files.size
        println(s"Analyzing $fileCount file(s) from specified path file...")

        files.toArray
      } catch {
        case _: Throwable =>
          println(s"Error while reading paths from $path"); Array()
      }
    } else {
      // We now assume that the given path refers to a directory
      println(s"Analyszing files from directory $path...")
      pathFile.listFiles(getOnlyFilesToAnalyzeFilter(analysisMode))
    }
  }

  def filePathMatchesAnalysisMode(
                                   filePath: String,
                                   analysisMode: AnalysisMode
                                 ): Boolean = {
    analysisMode match {
      case AnalyzeFromJAR => filePath.endsWith(".jar")
      case AnalyzeFromAPK => filePath.endsWith(".apk")
      case _              => filePath.endsWith(".jar") || filePath.endsWith(".apk")
    }
  }

  def setOutputStream(customOutputPath: Option[String]): Unit = {
    if (customOutputPath.isEmpty) {
      outputSteam = new FileWriter(getStandardOutputFile, false)
    } else {
      val outputPathFile = new File(customOutputPath.get)
      if (!outputPathFile.exists()) outputPathFile.createNewFile()
      outputSteam = new FileWriter(outputPathFile, false)
    }
  }

  def analyzeAppFromFile(appFile: File, analysisMode: AnalysisMode): Unit = {
    println("Analyzing " + appFile.getPath)
    analysisMode match {
      case AnalyzeFromJAR => analyzeJAR(appFile)
      case AnalyzeFromAPK => analyzeAPK(appFile)
      case AnalyzeFromAny => {
        if (appFile.getPath.endsWith(".jar")) analyzeJAR(appFile)
        else if (appFile.getPath.endsWith(".apk")) analyzeAPK(appFile)
      }
    }
  }

  def analyzeJAR(jarFile: File): Unit = {
    val jarName = jarFile.getAbsolutePath.split("/").last

    val androidLib = new File(AndroidJarAnalysis.identifyAndroidJar(jarFile))
    val project : Project[URL] = Project(Array(jarFile), Array(stdLib, androidLib))

    val (classLoaderInstantiations, instantiatedClassLoaderTypes) =
      new ClassLoaderFinder(project).findClassLoaderInstantiationsAndVariety()

    if (classLoaderInstantiations.nonEmpty) {
      logClassLoaderInstantiationResult(
        jarName,
        classLoaderInstantiations.size,
        instantiatedClassLoaderTypes.size
      )
    }
  }

  def analyzeAPK(apkFile: File): Unit = {
    println("Analyzing APK...")
    val apkManager = new APKManager(apkFile.getPath)
    val jarFile = new File(apkManager.pathToJAR)
    analyzeJAR(jarFile)
  }

  def logClassLoaderInstantiationResult(
      appFileName: String,
      instantiationAmount: Int,
      instantiatedClassLoaderTypeAmount: Int
  ): Unit = {
    outputSteam.write(
      s"$appFileName: \t \t $instantiationAmount instantiations of $instantiatedClassLoaderTypeAmount different types\n"
    )
  }

  def getOnlyFilesToAnalyzeFilter(analysisMode: AnalysisMode): FileFilter = {
    (file: File) =>
      {
        val path = file.getPath
        analysisMode match {
          case AnalyzeFromJAR => path.endsWith(".jar")
          case AnalyzeFromAPK => path.endsWith(".apk")
          case AnalyzeFromAny => path.endsWith(".jar") || path.endsWith(".apk")
        }
      }
  }

  def getStandardOutputFile: File = {
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
  val AnalyzeFromJAR, AnalyzeFromAPK, AnalyzeFromAny = Value
}

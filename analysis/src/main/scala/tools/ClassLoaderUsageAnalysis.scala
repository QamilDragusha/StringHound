package tools

import com.github.tototoshi.csv.CSVWriter
import helper.{APKManager, AndroidJarAnalysis, ClassLoaderFinder}
import main.StringDecryption
import main.StringDecryption.{ErrorLogger, outputDir, stdLib}
import org.apache.commons.cli.{DefaultParser, Options}
import org.apache.commons.io.IOUtils
import org.opalj.br.ObjectType
import org.opalj.br.analyses.Project
import org.opalj.log.{GlobalLogContext, OPALLogger}
import tools.AnalysisMode.{
  AnalysisMode,
  AnalyzeFromAPK,
  AnalyzeFromAny,
  AnalyzeFromJAR,
  AnalyzeLibrary
}

import java.io.{
  BufferedReader,
  File,
  FileFilter,
  FileOutputStream,
  FileReader,
  FileWriter,
  PrintWriter
}
import java.net.URL
import java.util.zip.ZipFile
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**  Analyzes apps of a given directory on whether they are using ClassLoaders.
  *
  *  A more detailled usage guide can be found in the tools.md file
  */
object ClassLoaderUsageAnalysis {

  private var outputDirectory: File = _
  private var verbose = false
  private var amountOfSuccessfullyAnalyzedFiles = 0

  private lazy val outputDirectoryPath: String = outputDirectory.getPath

  def main(args: Array[String]): Unit = {
    OPALLogger.updateLogger(GlobalLogContext, ErrorLogger)
    val (path, analysisMode) = parseAndSetUserDefinedArgs(args)

    println(s"Analyzing from $path...")

    val filesToAnalyze = determineFilesToAnalyze(path, analysisMode)
    filesToAnalyze.par.foreach(analyzeFile(_, analysisMode))

    createLog(filesToAnalyze.length)

    println("Terminating analysis...")

  }

  /** Parses the given arguments and returns a Tuple of (the path to the file/directory to analyze, the desired analysis mode)
    *  with which the analysis should be performed with. Also sets the output stream to a custom output path if specified.
    */
  private def parseAndSetUserDefinedArgs(
      args: Array[String]
  ): (String, AnalysisMode) = {

    val jarOption = "jar"
    val apkOption = "apk"
    val libraryOption = "lib"
    val pathOption = "p"
    val outputOption = "o"
    val verboseOption = "v"

    val options = new Options()

    options.addOption(
      libraryOption,
      false,
      "Analyze given libraries which are either in aar format or another"
    )

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
    options.addOption(
      verboseOption,
      "verbose",
      false,
      "Setting this flag will yield more verbose outputs"
    )

    val commandLineParser = new DefaultParser()
    val commandLine = commandLineParser.parse(options, args)

    val path: String = commandLine.getOptionValue(pathOption)

    val customOutputPath: Option[String] =
      if (commandLine.hasOption(outputOption))
        Some(commandLine.getOptionValue(outputOption))
      else None

    setOutputDirectory(customOutputPath)

    assert(
      !commandLine.hasOption(jarOption) || !commandLine.hasOption(apkOption),
      "Setting both -jar and -apk ignores every file"
    )

    if (commandLine.hasOption(verboseOption)) verbose = true

    if (commandLine.hasOption(jarOption)) {
      (path, AnalysisMode.AnalyzeFromJAR)
    } else if (commandLine.hasOption(apkOption)) {
      (path, AnalysisMode.AnalyzeFromAPK)
    } else if (commandLine.hasOption(libraryOption)) {
      (path, AnalysisMode.AnalyzeLibrary)
    } else {
      (path, AnalysisMode.AnalyzeFromAny)
    }

  }

  private def determineFilesToAnalyze(
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

  private def filePathMatchesAnalysisMode(
      filePath: String,
      analysisMode: AnalysisMode
  ): Boolean = {
    analysisMode match {
      case AnalyzeFromJAR => filePath.endsWith(".jar")
      case AnalyzeFromAPK => filePath.endsWith(".apk")
      case AnalyzeLibrary =>
        filePath.endsWith(".jar") || filePath.endsWith(".aar")
      case _ => filePath.endsWith(".jar") || filePath.endsWith(".apk")
    }
  }

  private def setOutputDirectory(customOutputPath: Option[String]): Unit = {
    if (customOutputPath.isEmpty) {
      outputDirectory = getStandardOutputDirectory
    } else {
      val outputPathFile = new File(customOutputPath.get)
      if (!outputPathFile.exists()) outputPathFile.mkdir()
      outputDirectory = outputPathFile
    }
  }

  private def analyzeFile(
      fileToAnalyze: File,
      analysisMode: AnalysisMode
  ): Unit = {
    logIfVerbose("Analyzing " + fileToAnalyze.getPath)

    try {
      analysisMode match {
        case AnalyzeFromJAR => analyzeJAR(fileToAnalyze)
        case AnalyzeFromAPK => analyzeAPK(fileToAnalyze)
        case AnalyzeFromAny => {
          if (fileToAnalyze.getPath.endsWith(".jar")) analyzeJAR(fileToAnalyze)
          else if (fileToAnalyze.getPath.endsWith(".apk"))
            analyzeAPK(fileToAnalyze)
        }
        case AnalyzeLibrary => {
          if (fileToAnalyze.getPath.endsWith(".jar")) analyzeJAR(fileToAnalyze)
          else if (fileToAnalyze.getPath.endsWith(".aar"))
            analyzeAAR(fileToAnalyze)
        }
      }
      amountOfSuccessfullyAnalyzedFiles += 1
    }
  }

  private def analyzeAAR(aarFile: File): Unit = {
    logIfVerbose(s"Analyzing given aar ${aarFile.getPath} ...")
    val aarFileArchive = new ZipFile(aarFile.getPath)
    val archiveEntries = aarFileArchive.entries()

    while (archiveEntries.hasMoreElements) {
      val entryUnderInspection = archiveEntries.nextElement()
      if (entryUnderInspection.getName.endsWith(".jar")) {

        val entryInput = aarFileArchive.getInputStream(entryUnderInspection)

        val tempJarFile =
          File.createTempFile(entryUnderInspection.getName, null)
        tempJarFile.deleteOnExit()

        val entryToTempOutput = new FileOutputStream(tempJarFile);
        IOUtils.copy(entryInput, entryToTempOutput)
        analyzeJAR(tempJarFile)
      }
    }
  }

  private def analyzeJAR(jarFile: File): Unit = {
    val jarName = jarFile.getAbsolutePath.split("/").last

    val project: Project[URL] = Project(jarFile)

    val (instanciationAmountByClassLoader, absoluteSumOfClassLoaderInstances) =
      new ClassLoaderFinder(project).computeClassLoaderVariety()

    if (absoluteSumOfClassLoaderInstances > 0)
      logIfVerbose(
        s"Found $absoluteSumOfClassLoaderInstances classLoaders: ${jarFile.getPath}",
        highlight = true
      )

    logClassLoaderInstantiationResult(
      jarName,
      instanciationAmountByClassLoader,
      absoluteSumOfClassLoaderInstances
    )
  }

  private def analyzeAPK(apkFile: File): Unit = {
    logIfVerbose("Analyzing APK...")
    val apkManager = new APKManager(apkFile.getPath)
    val jarFile = new File(apkManager.pathToJAR)
    analyzeJAR(jarFile)
  }

  private def logClassLoaderInstantiationResult(
      appFileName: String,
      instantiationAmountByClassLoader: mutable.LinkedHashMap[ObjectType, Int],
      absoluteSumOfClassLoaderInstances: Int
  ): Unit = {
    val csvResultWriter = CSVWriter.open(createOutputFile(appFileName))

    csvResultWriter.writeRow(Seq("Sum", absoluteSumOfClassLoaderInstances))
    instantiationAmountByClassLoader.foreach {
      classLoaderTypeAndInstanceAmount =>
        csvResultWriter.writeRow(
          Seq(
            classLoaderTypeAndInstanceAmount._1,
            classLoaderTypeAndInstanceAmount._2
          )
        )
    }

    csvResultWriter.close()
  }

  private def createOutputFile(appFileName: String): File = {
    val outputFilePath = s"$outputDirectoryPath/$appFileName.csv"
    new File(outputFilePath)
  }

  private def getOnlyFilesToAnalyzeFilter(
      analysisMode: AnalysisMode
  ): FileFilter = { (file: File) =>
    {
      val path = file.getPath
      analysisMode match {
        case AnalyzeFromJAR => path.endsWith(".jar")
        case AnalyzeFromAPK => path.endsWith(".apk")
        case AnalyzeLibrary => path.endsWith(".jar") || path.endsWith(".aar")
        case AnalyzeFromAny => path.endsWith(".jar") || path.endsWith(".apk")
      }

    }
  }

  private def getStandardOutputDirectory: File = {
    val pathToOutputDir = StringDecryption.outputDir + "/classLoaderAnalysis/"
    val outputDir = new File(pathToOutputDir)
    if (!outputDir.exists()) outputDir.mkdir()
    outputDir
  }

  private def logIfVerbose(obj: Any, highlight: Boolean = false): Unit = if (
    verbose
  ) {
    if (highlight) { println(Console.MAGENTA + obj + Console.RESET) }
    else { println(obj) }
  }

  private def createLog(totalAmountOfFiles: Int): Unit = {
    val logFilePath = s"$outputDirectoryPath/log.txt"
    val logFile = new File(logFilePath)
    if (!logFile.exists()) logFile.createNewFile()
    val logPrintWriter = new PrintWriter(new FileWriter(logFile))
    val performanceInPercent = if (totalAmountOfFiles > 0) {
      100 * amountOfSuccessfullyAnalyzedFiles / totalAmountOfFiles
    } else { 0 }
    logPrintWriter.print(
      s"Analyzed $totalAmountOfFiles files. $amountOfSuccessfullyAnalyzedFiles of $totalAmountOfFiles were succesfully analyzed ($performanceInPercent%)"
    )
    logPrintWriter.close()
  }

}

object AnalysisMode extends Enumeration {
  type AnalysisMode = Value
  val AnalyzeFromJAR, AnalyzeFromAPK, AnalyzeLibrary, AnalyzeFromAny = Value
}

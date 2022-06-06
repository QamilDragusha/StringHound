package tools

import analyses.SlicingClassAnalysis
import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import helper.{APKManager, ClassLoaderFinder, LoggingLevel, PrintLog}
import main.Deobfuscator.ErrorLogger
import org.apache.commons.cli.{CommandLine, DefaultParser, Options}
import org.opalj.br.ObjectType
import org.opalj.br.analyses.Project
import org.opalj.log.{GlobalLogContext, OPALLogger}

import java.io.File
import java.util.concurrent.{ForkJoinPool, atomic}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.mutable.ParArray
import scala.io.AnsiColor._

/*
  Evaluates the recall of the class deobfuscation, namely
  "of how many of the found dynamic code loading instances were we able to
  correctly deobfuscate the class?"
 */
object ClassDeobfuscationBenchmark {

  private val options = initializeOptions()
  private var commandLine: CommandLine = _

  private val pathsFilePathOption = "pathsFilePath"
  private val basePathOption = "basePath"
  private val logPathOption = "logPath"
  private val summaryDirectoryOption = "summaryDirectory"

  private val totalAnalyzed = new AtomicInteger()
  private val totalAnalyzedUsages = new AtomicInteger()

  private val erroneosUsageAnalyses = new AtomicInteger()

  private val forkJoinPool = new ForkJoinPool(2)

  lazy val logPath: String = commandLine.getOptionValue(logPathOption)
  lazy val pathsFilePath: String =
    commandLine.getOptionValue(pathsFilePathOption)
  lazy val basePath: String =
    extractDirFromCommandLine(basePathOption)
  lazy val summaryDirectory: String =
    extractDirFromCommandLine(summaryDirectoryOption)


  def main(args: Array[String]): Unit = {
    OPALLogger.updateLogger(GlobalLogContext, ErrorLogger)
    PrintLog.mute()
    initializeCommandLine(args)
    val filePaths = readAllPathsFromPathsFile()
    analyzeUsagesAndReveals(filePaths)
    val errors = erroneosUsageAnalyses.get()
    if (errors > 0) {
      println(s"Encountered $errors errors")
    }
  }

  private def extractDirFromCommandLine(commandLineOption: String): String = {
    var path = commandLine.getOptionValue(commandLineOption)
    println(commandLineOption)
    if (!path.endsWith("/")) path += "/"
    path
  }

  def toParArray[T](array: Array[T]): ParArray[T] = {
    val parArray = array.par
    parArray.tasksupport = new ForkJoinTaskSupport(forkJoinPool)
    parArray
  }

  def logTotalAnalysisStepCounter(): Unit = {
    val total = totalAnalyzed.incrementAndGet()
    println(s"Analyzing $total")
  }

  def logUsageAnalysisStepCounter(): Unit = {
    val total = totalAnalyzedUsages.incrementAndGet()
    println(s"Analyzing usages $total")
  }

  private def initializeCommandLine(args: Array[String]): Unit = {
    commandLine = new DefaultParser().parse(options, args)
  }

  private def initializeOptions(): Options = {
    val options = new Options()
    options.addRequiredOption(
      pathsFilePathOption,
      "pathsFilePath",
      true,
      "The path to the file containing all filepaths"
    )
    options.addRequiredOption(
      basePathOption,
      "basePath",
      true,
      "The path to which the file paths are relative to"
    )
    options.addRequiredOption(
      logPathOption,
      "logPath",
      true,
      "The path of the log file to be created"
    )

    options.addRequiredOption(
      summaryDirectoryOption,
      "summaryDirectory",
      true,
      "The directory of the summaries to be created"
    )
    options
  }

  private def readAllPathsFromPathsFile(): Array[String] = {
    val pathsFile = new File(pathsFilePath)
    if (!pathsFile.exists()) {
      class PathsFileNotExistentException extends RuntimeException
      throw new PathsFileNotExistentException()
    }
    val pathsFileReader = CSVReader.open(pathsFile)
    pathsFileReader.iterator.flatten
      .filter(relativeFilePath => {
       !new File(s"$summaryDirectory$relativeFilePath.csv").exists()
      })
      .map(relativeFilePath => s"$basePath$relativeFilePath")
      .toArray
  }

  private def analyzeUsagesAndReveals(
      paths: Array[String]
  ): Unit = {

    toParArray(paths).foreach(filePath => {
      logUsageAnalysisStepCounter()
      val appFile = new File(filePath)
      if (!appFile.exists()) {
        println(s"AppFile $filePath not found!")
      } else {
        val (usageCharacteristics, totalUsages) =
          getSingleAppsClassLoaderUsages(appFile)
        if (totalUsages > 0) {
          try {
            val (revealCharacteristics, _) =
              getSingleAppsClassLoaderAnalysisCharacteristics(appFile)

            logTotalAnalysisStepCounter()

            val relativeFilePath = filePath.replaceFirst(basePath, "")
            saveApplicationSummary(usageCharacteristics, revealCharacteristics, relativeFilePath)

          } catch {
            case _: Exception => erroneosUsageAnalyses.incrementAndGet()
          }
        }
      }

    })
  }

  private def saveApplicationSummary(usageCharacteristics: mutable.Map[ObjectType, Int], foundCharacteristics: mutable.Map[ObjectType, Int], relativeAppFilePath: String) = {
    val summaryFile = new File(s"$summaryDirectory$relativeAppFilePath.csv")
    summaryFile.createNewFile()
    val summaryWriter = CSVWriter.open(summaryFile)

    usageCharacteristics.keySet.foreach(objectType => {
      summaryWriter.writeRow(Seq(objectType.fqn, usageCharacteristics(objectType), foundCharacteristics.getOrElse(objectType, 0)))
    })

    summaryWriter.close()
  }

  private def getSingleAppsClassLoaderUsages(
      appFile: File
  ): (mutable.Map[ObjectType, Int], Int) = {
    try {
      val apkManager = new APKManager(appFile.getPath)
      val jarFile = new File(apkManager.pathToJAR)
      val classLoaderFinder = new ClassLoaderFinder(Project(jarFile))
      val result = classLoaderFinder.computeClassLoaderVariety()
      result
    } catch {
      case _: Exception =>
        print("Exception")
        erroneosUsageAnalyses.incrementAndGet()
        (new mutable.LinkedHashMap[ObjectType, Int](), 0)
    }
  }

  private def getSingleAppsClassLoaderAnalysisCharacteristics(
      appFile: File
  ): (mutable.Map[ObjectType, Int], Int) = {
    val absolutePath = appFile.getAbsolutePath
    val parameters = List(appFile.getName, absolutePath)
    val classLoaderSlicingAnalysis =
      new SlicingClassAnalysis(appFile.getPath, parameters)
    classLoaderSlicingAnalysis.doAnalyze(
      System.currentTimeMillis(),
      false,
      false,
      true
    )
    val characteristics =
      classLoaderSlicingAnalysis.successfullyAnalyzedCharacteristics
    println(characteristics)
    var totalFoundClassLoaders = 0
    characteristics.values.foreach(value => totalFoundClassLoaders += value)
    println("Total: " + totalFoundClassLoaders)
    (characteristics, totalFoundClassLoaders)
  }

}

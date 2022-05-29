package tools

import analyses.SlicingClassAnalysis
import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import helper.ClassLoaderFinder
import main.StringDecryption.ErrorLogger
import org.apache.commons.cli.{CommandLine, DefaultParser, Options}
import org.opalj.br.ObjectType
import org.opalj.br.analyses.Project
import org.opalj.log.{GlobalLogContext, OPALLogger}

import java.io.File
import java.net.URL
import java.util.concurrent
import java.util.concurrent.{ConcurrentHashMap, atomic}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.JavaConverters.mapAsScalaMapConverter
import scala.collection.mutable

/*
  Evaluates the recall of the class deobfuscation, namely
  "of how many of the found dynamic code loading instances were we able to
  correctly deobfuscate the class?"
 */
object ClassDeobfuscationEvaluation {

  private val options = initializeOptions()
  private var commandLine: CommandLine = _

  private val pathsFilePathOption = "pathsFilePath"
  private val basePathOption = "basePath"
  private val logPathOption = "logPath"

  private val erroneosUsageAnalyses = new AtomicInteger()

  def main(args: Array[String]): Unit = {
    OPALLogger.updateLogger(GlobalLogContext, ErrorLogger)
    initializeCommandLine(args)
    val filePaths = readAllPathsFromPathsFile()
    println("Evaluating apps...")
    val results = evaluate(filePaths)
    println("Writing results")
    writeResults(results)
    val errors = erroneosUsageAnalyses.get()
    if (errors > 0) {
      println(s"Encountered $errors errors")
    }
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
    options
  }

  private def readAllPathsFromPathsFile(): Array[String] = {
    val pathToPathsFile : String = commandLine.getOptionValue(pathsFilePathOption)
    println(pathToPathsFile)
    var basePath = commandLine.getOptionValue(basePathOption)
    println(basePath)

    if (!basePath.endsWith("/")) basePath += "/"

    val pathsFile = new File(pathToPathsFile)
    if (!pathsFile.exists()) {
      class PathsFileNotExistentException extends RuntimeException
      throw new PathsFileNotExistentException()
    }
    val pathsFileReader = CSVReader.open(pathsFile)
    pathsFileReader.iterator.flatten
      .map(relativeFilePath => s"$basePath$relativeFilePath")
      .toArray
  }

  private def evaluate(paths: Array[String]) : mutable.Map[ObjectType, (Int, Int)] = {
    val (totalCharacteristics, totalRevealedCharacteristics) = analyzeUsagesAndReveals(paths)
    val result : mutable.HashMap[ObjectType, (Int, Int)] = new mutable.HashMap()
    totalCharacteristics.keySet.foreach(
      key => {
          result += {key -> Tuple2(totalCharacteristics(key), totalRevealedCharacteristics.getOrElse(key, 0))}
      }
    )
    result
  }

  private def analyzeUsagesAndReveals(
      paths: Array[String]
  ): (mutable.Map[ObjectType, Int], mutable.Map[ObjectType, Int]) = {
    val totalCharacteristics = new ConcurrentHashMap[ObjectType, Int]()
    val totalRevealedCharacteristics = new ConcurrentHashMap[ObjectType, Int]()
    paths.par.foreach(filePath => {
      val appFile = new File(filePath)
      if (!appFile.exists()) {
        println(s"AppFile $filePath not found!")
      } else {
        println(s"Analyzing $filePath")
        val (usageCharacteristics, totalUsages) =
          getSingleAppsClassLoaderUsages(appFile)
        println(totalUsages)
        if (totalUsages > 0) {
          try {
            val (revealCharacteristics, revealedTotal) = getSingleAppsClassLoaderAnalysisCharacteristics(appFile)


            usageCharacteristics.par.foreach(entry =>
            {
              val (objectType, amount) = entry
              totalCharacteristics.replace(
                objectType,
                totalCharacteristics.getOrDefault(objectType, 0) + amount
              )
            }
            )

            revealCharacteristics.par.foreach(entry => {
              val (objectType, amount) = entry
              totalRevealedCharacteristics.replace(
                objectType,
                totalRevealedCharacteristics.getOrDefault(objectType, 0) + amount
              )
            })
          }
          catch {
            case _ : Exception => erroneosUsageAnalyses.incrementAndGet()
          }

        }
      }

    })
    (totalCharacteristics.asScala, totalRevealedCharacteristics.asScala)
  }

  private def getSingleAppsClassLoaderUsages(
      appFile: File
  ): (mutable.Map[ObjectType, Int], Int) = {
    try {
      val classLoaderFinder = new ClassLoaderFinder(Project(appFile))
      val result = classLoaderFinder.computeClassLoaderVariety()
      println(result)
      result
    } catch {
      case _: Exception =>
        println("Exception")
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
    var totalFoundClassLoaders = 0
    characteristics.values().forEach(value => totalFoundClassLoaders += value)
    (characteristics.asScala, totalFoundClassLoaders)
  }

  private def writeResults(results: mutable.Map[ObjectType, (Int, Int)]) : Unit = {
    val logPath = commandLine.getOptionValue(logPathOption)
    val logFile = new File(logPath)
    logFile.createNewFile()
    val logWriter = CSVWriter.open(logFile)
    results.foreach(resultEntry => {
      val objectType = resultEntry._1
      val (totalClasses, revealedClasses) = resultEntry._2
      logWriter.writeRow(Seq(objectType.fqn,totalClasses, revealedClasses))
    })
    logWriter.close()
  }

}

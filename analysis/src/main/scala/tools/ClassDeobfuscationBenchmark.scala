package tools

import analyses.SlicingClassAnalysis
import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import helper.{APKManager, ClassLoaderFinder, PrintLog}
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
object ClassDeobfuscationBenchmark {

  private val options = initializeOptions()
  private var commandLine: CommandLine = _

  private val pathsFilePathOption = "pathsFilePath"
  private val basePathOption = "basePath"
  private val logPathOption = "logPath"

  private val erroneosUsageAnalyses = new AtomicInteger()

  def main(args: Array[String]): Unit = {
    OPALLogger.updateLogger(GlobalLogContext, ErrorLogger)
    PrintLog.mute()
    initializeCommandLine(args)
    val filePaths = readAllPathsFromPathsFile()
    val results = evaluate(filePaths)
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
    val pathToPathsFile: String =
      commandLine.getOptionValue(pathsFilePathOption)
    var basePath = commandLine.getOptionValue(basePathOption)

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

  private def evaluate(
      paths: Array[String]
  ): mutable.Map[ObjectType, (Int, Int)] = {
    val (totalCharacteristics, totalRevealedCharacteristics) =
      analyzeUsagesAndReveals(paths)
    val totalAndCustomUsagesByType: mutable.HashMap[ObjectType, (Int, Int)] = new mutable.HashMap()
    totalCharacteristics.keySet.foreach(objectType => {
      totalAndCustomUsagesByType += {
        objectType -> Tuple2(
          totalCharacteristics(objectType),
          totalRevealedCharacteristics.getOrElse(objectType, 0)
        )
      }
    })
    totalAndCustomUsagesByType
  }


  private def analyzeUsagesAndReveals(
      paths: Array[String]
  ): (mutable.Map[ObjectType, Int], mutable.Map[ObjectType, Int]) = {
    var totalCharacteristics : mutable.Map[ObjectType, Int] = new mutable.HashMap()
    var totalRevealedCharacteristics: mutable.Map[ObjectType, Int] = new mutable.HashMap()
    paths.par.foreach(filePath => {
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

            // summarize resulting characteristics. Could cause race
            // conditions without synchronization
            synchronized {
              totalCharacteristics = unsafelyCopyAndUpdateCharacteristics(totalCharacteristics, usageCharacteristics)
            } // Splitting up synchonized blocks increases performance
            synchronized{
              totalRevealedCharacteristics = unsafelyCopyAndUpdateCharacteristics(totalRevealedCharacteristics, revealCharacteristics)
            }

          } catch {
            case _: Exception => erroneosUsageAnalyses.incrementAndGet()
          }
        }
      }

    })
    (totalCharacteristics, totalRevealedCharacteristics)
  }

  private def unsafelyCopyAndUpdateCharacteristics(
                                                    characteristicToUpdate: mutable.Map[ObjectType, Int],
                                                    update: mutable.Map[ObjectType, Int]
                                                  ): mutable.Map[ObjectType, Int] = {
    val characteristics = characteristicToUpdate.clone()
    update.foreach(updateEntry => {
      val (objectTypeToUpdate, amountToAdd) = updateEntry
      val currentAmount =  characteristics.get(objectTypeToUpdate)
      if (currentAmount.isDefined) {
        characteristics.update(objectTypeToUpdate, currentAmount.get + amountToAdd)
      } else {
        characteristics += {objectTypeToUpdate -> amountToAdd}
      }
    })
    characteristics
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

  private def writeResults(
      results: mutable.Map[ObjectType, (Int, Int)]
  ): Unit = {
    val logPath = commandLine.getOptionValue(logPathOption)
    val logFile = new File(logPath)
    logFile.createNewFile()
    val logWriter = CSVWriter.open(logFile)
    results.foreach(resultEntry => {
      val objectType = resultEntry._1
      val (totalClasses, revealedClasses) = resultEntry._2
      logWriter.writeRow(Seq(objectType.fqn, totalClasses, revealedClasses))
    })
    logWriter.close()
  }

}

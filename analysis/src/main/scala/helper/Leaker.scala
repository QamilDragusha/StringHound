package helper

import models.ClassSlicingContext
import org.opalj.br.ObjectType

import java.io.{File, FileOutputStream}
import java.nio.ByteBuffer
import java.lang.reflect.Field
import scala.collection.mutable

object Leaker {

}

class Leaker(apkManager: APKManager) {

  private var highestUnusedFileEnumeration : Int = 0

  private val usedFileNames : mutable.HashSet[String] = new mutable.HashSet[String]()
  private val resultsDirectory = apkManager.resultsDirectory

  def leakResult(resultField: Field, context: ClassSlicingContext) : Unit = {
    val fieldObjectType = context.dataTypeOfInterest
    fieldObjectType match {
      case ObjectType("java/nio/ByteBuffer") =>
        {
          val result = resultField.get(null).asInstanceOf[ByteBuffer]
          leakDexFile(result)
        }
      case _ => return
    }
  }

  def leakDexFile(dexBuffer: ByteBuffer, preferredName: Option[String] = None) : Unit = {
    val fileName = getSuitableDexFileName(preferredName)
    val filePath = s"${resultsDirectory.getPath}/$fileName"

    println("Leaking file at " + filePath + ", with it's resultsDirectory being " + resultsDirectory.getPath)

    val file = new File(filePath)

    file.createNewFile()

    val outputChannel = new FileOutputStream(file, true).getChannel
    outputChannel.write(dexBuffer)
    outputChannel.close()
  }

  private def getSuitableDexFileName(preferredName: Option[String]) : String = {
    var possibleFileName : String = null

    if (preferredName.isDefined) {
       possibleFileName = preferredName.get
      if (!usedFileNames.contains(possibleFileName) && !possibleFileName.contains("/")) {
        if(!possibleFileName.endsWith(".dex")) {
          possibleFileName += ".dex"
        }
        usedFileNames.add(possibleFileName)
        return possibleFileName
      }
    }

    do {
      possibleFileName = s"leak_$highestUnusedFileEnumeration.dex"
      highestUnusedFileEnumeration += 1
    } while(usedFileNames.contains(possibleFileName))

    usedFileNames.add(possibleFileName)

    possibleFileName
  }

}

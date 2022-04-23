package helper

import java.io.File
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.XML

class AndroidManifestReader(decodedAPKDirectory : File) {

  private val pathToManifest = decodedAPKDirectory.getPath + "/AndroidManifest.xml"
  private val pathToValueDir = decodedAPKDirectory.getPath + "/res/values"
  private val pathToIntegers = s"$pathToValueDir/integers.xml"
  private val pathToStrings = s"$pathToValueDir/strings.xml"

  // TODO: Refactor to gracefully handle errors
  private val androidManifest = XML.loadFile(pathToManifest)
  private val integers = XML.loadFile(pathToIntegers)
  private val strings = XML.loadFile(pathToStrings)

  // TODO: Search for specific node, for some reason \ "@android:value" or \ "@android:name" does not work
  lazy private val metaDataNodes = androidManifest \ "application" \ "meta-data"
  lazy private val integersNodes = integers \\ "integer"

  def gracefullyReadApplicationMetaDataInteger(valueName: String, onError: Integer = 0) : Integer = {
    try {
      readApplicationMetaDataInteger(valueName)
    } catch {
      case _ : Throwable => onError
    }
  }

  def readApplicationMetaDataInteger(valueName: String) : Integer = {
    val nodesMatchingValueName = metaDataNodes filter {node => node.toString().contains(valueName)}
    if (nodesMatchingValueName.nonEmpty) {
      val metaDataNode = nodesMatchingValueName.head
      val valuePattern = "android:value=\"[^\"]*\"".r
      val value = valuePattern.findFirstIn(metaDataNode.toString()).map(value => value.split('"')(1))

      if (value.isDefined) {
        val integerName = value.get.split("/")(1)
        val valueNode = integersNodes.filter(node => node.toString().contains(integerName))
        if (valueNode.nonEmpty) {
          return Integer.parseInt(valueNode.text)
        }
      }
    }

    class ManifestValueNotFoundException extends RuntimeException

    throw new ManifestValueNotFoundException()
  }


}

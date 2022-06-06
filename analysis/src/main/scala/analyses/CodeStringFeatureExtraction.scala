package analyses

import java.io.{File, FileWriter}
import java.net.URL

import classifier.StringClassifier
import main.Deobfuscator
import org.opalj.br.analyses.{Project, StringConstantsInformationKey}


class CodeStringFeatureExtraction(val project: Project[URL], val parameters: Seq[String], filterSet: Set[String]) {


  def doAnalyze(): Unit = {

    val resultStream = new FileWriter(new File(Deobfuscator.outputDir + "/results/" + parameters.head + ".txt"), false)


    val stringUsages = project.get(StringConstantsInformationKey)
    for (string <- stringUsages.filter(p => !filterSet.contains(p._1.replaceAll("[\n\r,]", ""))).par) {
      try {
        resultStream.write((List(string._1.replaceAll("[,\n\r]", "")) ++ StringClassifier.getStatistics(string._1)).mkString(",") + (if (filterSet.isEmpty) ",0" else ",1") + "\n")
        resultStream.flush()
      } catch {
        case e: Throwable => Deobfuscator.logger.error(parameters.head)
          Deobfuscator.logger.error(e.getMessage)
      }
    }

    resultStream.close()

  }

}

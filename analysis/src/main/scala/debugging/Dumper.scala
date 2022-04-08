package debugging

import main.StringDecryption
import org.opalj.br.{ClassFile, Code, Method, MethodTemplate}

import java.io.{File, FileWriter}

object Dumper {

  /// Preference flags to configure the dumping
  private val allowAnyDumping = true


  private val classFiles = false
  private val slicedMethods = false


  private val arbitraryMethods = true
  private val methodTemplates = true
  private val codeDirectly = true
  private val arbitraryClassFiles = false

  private var uniqueId : Int = 0

  private def makeAllDirectories(subdirectories: String = null) : Unit = {
    new File(StringDecryption.outputDir + "/results/").mkdir()
    new File(StringDecryption.outputDir + "/results/debugging/").mkdir()

    if (subdirectories != null) return

    var directory = StringDecryption.outputDir + "results/debugging/"

    val splittedDirs = subdirectories.replaceFirst(directory, "").split("/")

    for (subDirectory <- splittedDirs) {
      directory = directory + subDirectory + "/"
      new File(directory).mkdir()
    }
  }

  private def getFileDirectory(subDirectory: String) : String = {
    StringDecryption.outputDir + "/results/" + "debugging/" + subDirectory
  }

  def dumpCode(code: Code, fileName: String, resultSubDirectory: String = "code/", message: String = null, force: Boolean = false) : Unit = {
    if (!allowAnyDumping) return
    if (!codeDirectly && !force) return


    val fileDirectory = getFileDirectory(resultSubDirectory)

    makeAllDirectories(fileDirectory)

    new File(fileDirectory).mkdir()

    val normalizedFileName : String = fileName.replaceAll("/", "-")
    val dumpStream = new FileWriter(new File(fileDirectory + uniqueId + "--" +  normalizedFileName + ".txt"), false)

    if (message != null) {
      dumpStream.append(message + "\n")
    }

    code.map {pcAndInstruction =>
      dumpStream.append(pcAndInstruction.pc + ": " + pcAndInstruction.instruction + "\n")
      dumpStream.flush()
    }
    dumpStream.close()

    uniqueId += 1

  }

  def dumpMethod(method: Method, resultSubDirectory: String = "methods/", message: String = null) : Unit = {
    if (!allowAnyDumping) return
    if (!arbitraryMethods ) return
    val fileName =  method.classFile.fqn + "--FROM--" +  method.name


    dumpCode(method.body.get, fileName, resultSubDirectory, message, true)

  }

  def dumpMethodTemplate(methodTemplate: MethodTemplate, resultSubDirectory: String = "methodTemplates/", message: String = null) : Unit = {
    if (!allowAnyDumping) return
    if (!methodTemplates) return

    val fileName = methodTemplate.name

    dumpCode(methodTemplate.body.get, fileName, resultSubDirectory, message, force = true)
  }

  def dumpClassFile(classFile: ClassFile, resultSubDirectory: String = "classFiles/", message: String = null) : Unit = {
    if (!allowAnyDumping) return
    if (!arbitraryClassFiles) return

    val fileDirectory = getFileDirectory(resultSubDirectory)

    makeAllDirectories(fileDirectory)

    new File(fileDirectory).mkdir()

    val fileName = uniqueId + "--" + classFile.fqn
    uniqueId += 1

    val normalizedFileName : String = fileName.replaceAll("/", "-")
    val dumpStream = new FileWriter(new File(fileDirectory + uniqueId + "--" +  normalizedFileName + ".txt"), false)

    val classSignature = classFile.classSignature

    if (message != null) {
      dumpStream.append(message + "\n\n").flush()
    }


    if (classSignature isDefined) {
      dumpStream.append("CLASSSIGNATURE: \n\n\n" + classSignature.get + "\n\n\n").flush()
    }

    classFile.constructors.foreach{ constructor => {
      val constructorCode = constructor.body
      if (constructorCode isDefined) {
        dumpStream.append("\n" + constructor.name + "\n")
        constructorCode.get.foreach{
          pcAndInstruction => dumpStream.append(pcAndInstruction.pc + ": " + pcAndInstruction.instruction + "\n")
        }
        dumpStream.append("\n")
        dumpStream.flush()
      }
    } }

    dumpStream.append("FIELDS: \n\n\n")
    dumpStream.append(classFile.fields.mkString("\n"))

    dumpStream.append("METHODS: \n\n\n")

    classFile.methods.foreach{ method => {
      val constructorCode = method.body
      if (constructorCode isDefined) {
        dumpStream.append("\n" + method.name + "\n")
        constructorCode.get.foreach{
          pcAndInstruction => dumpStream.append(pcAndInstruction.pc + ": " + pcAndInstruction.instruction + "\n")
        }
        dumpStream.append("\n")
        dumpStream.flush()
      }
    } }


    dumpStream.flush()
    dumpStream.close()
    println("Dumped Slice")
  }

  def dumpModifiedClassFile(modifiedClassFile: ClassFile, initialMethod: Method, resultSubDirectory: String = "slicedClassDumps/") : Unit = {
    if (!allowAnyDumping) return
    if (!classFiles) return

    val fileDirectory = StringDecryption.outputDir + "/results/" + resultSubDirectory
    val fileName = (modifiedClassFile.fqn + "--FROM--" +  initialMethod.name).replaceAll("/", "-")

    new File(fileDirectory).mkdir()

    val classDumpStream = new FileWriter(new File(fileDirectory + fileName + ".txt"), false)

    val classSignature = modifiedClassFile.classSignature

    if (classSignature isDefined) {
      classDumpStream.append("CLASSSIGNATURE: \n\n\n" + classSignature.get + "\n\n\n").flush()
    }

    classDumpStream.append("CONSTRUCTORS: \n\n\n")

    modifiedClassFile.constructors.foreach{ constructor => {
      val constructorCode = constructor.body
      if (constructorCode isDefined) {
        classDumpStream.append("\n" + constructor.name + "\n")
        constructorCode.get.foreach{
          pcAndInstruction => classDumpStream.append(pcAndInstruction.pc + ": " + pcAndInstruction.instruction + "\n")
        }
        classDumpStream.append("\n")
        classDumpStream.flush()
      }
    } }

    classDumpStream.append("FIELDS: \n\n\n")
    classDumpStream.append(modifiedClassFile.fields.mkString("\n"))

    classDumpStream.append("METHODS: \n\n\n")

    modifiedClassFile.methods.foreach{ method => {
      val constructorCode = method.body
      if (constructorCode isDefined) {
        classDumpStream.append("\n" + method.name + "\n")
        constructorCode.get.foreach{
          pcAndInstruction => classDumpStream.append(pcAndInstruction.pc + ": " + pcAndInstruction.instruction + "\n")
        }
        classDumpStream.append("\n")
        classDumpStream.flush()
      }
    } }


    classDumpStream.flush()
    classDumpStream.close()
    println("Dumped Slice")


  }


  def dumpSlicedMethod(slicedMethod: MethodTemplate, originalMethod: Method, resultSubDirectory: String = "slicedDumps/") : Unit = {
    if (!allowAnyDumping) return
    if (!slicedMethods) return
    if (slicedMethod.body isDefined) {
      println("Sliced result of " + originalMethod)
      val fileDirectory = StringDecryption.outputDir + "/results/" + resultSubDirectory
      val fileName = (originalMethod.classFile.fqn + "@" +  originalMethod.name).replaceAll("/", "-")
      new File(fileDirectory).mkdir()
      val slicedMethodStream = new FileWriter(new File(fileDirectory + fileName + ".txt"), false)
      val code = slicedMethod.body.get
      code.map {pcAndInstruction =>
        slicedMethodStream.append(pcAndInstruction.pc + ": " + pcAndInstruction.instruction + "\n")
        slicedMethodStream.flush()
      }
      slicedMethodStream.close()
      println("Dumped Slice")
    }
  }


}

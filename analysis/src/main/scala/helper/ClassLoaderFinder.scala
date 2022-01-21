package helper


import org.opalj.br.analyses.Project

object ClassLoaderFinder {

  def analyzeConcreteJAR(projectJARPath: String): Unit = {
    implicit val p =
      Project(new java.io.File(projectJARPath), org.opalj.bytecode.RTJar)
    var numReflectionCalls: Int = 0
    var numberOfLoadedClasses: Int = 0
    var dexClassLoaderReferences: Int = 0
    var classLoaderReferences: Int = 0

    p.allMethodsWithBody.foreach { method =>
    {
      val body = method.body
      if (body != None) {
        if (body.toString.contains("java.lang.reflect")) {
          numReflectionCalls += 1
        }
        if ((body toString).contains("dexclassloader")) {
          numberOfLoadedClasses += 1
          dexClassLoaderReferences += 1
        }
        if (body.toString.contains("java.lang.ClassLoader")) {
          if (body.get == None) {
            return;
          }
          ///
          body.get.foreach {
            pcAndInstruction => {
              val line = pcAndInstruction.instruction.toString(pcAndInstruction.pc)
              if (line.contains("DynamiteLoaderV2") || line.contains("HwNotchSizeUtil") ||line.contains("InstantAppsRuntime"))
                println("InstructionPrinted: " + pcAndInstruction.instruction.toString(pcAndInstruction.pc))
            }
          }
          /*
          body.get.foreach {
            pcAndInstruction => println("InstructionPrinted" + pcAndInstruction.instruction.toString(pcAndInstruction.pc))
          } // Output e.g. InstructionPrintedINVOKESPECIAL(androidx.fragment.app.Fragment$SavedState{ void <init>(android.os.Parcel,java.lang.ClassLoader) })
          */

          ///
          val methodInvokatopnInstructions = body.get.instructions.filter {
            instruction =>
              instruction != null && instruction.isMethodInvocationInstruction
          }
          val objectTypes = methodInvokatopnInstructions.filter {
            instruction =>
              instruction.asMethodInvocationInstruction.declaringClass.isObjectType
          }
          val classLoaderUses = objectTypes.filter { instruction =>
            instruction.asMethodInvocationInstruction.declaringClass.asObjectType.fqn
              .contains("java/lang/ClassLoader")
          }
          if (classLoaderUses.nonEmpty) {
            println(
              method.classFile.fqn + " => " + method.toJava + " => " + classLoaderUses.head
            )
          }

          numberOfLoadedClasses += 1
          classLoaderReferences += 1
        }

      }

    }
    }

    println(
      "Found " + numReflectionCalls + " reflection calls and " + numberOfLoadedClasses + " potential loaded classes"
    )
    println(
      "dexclassloader: " + dexClassLoaderReferences + " java.lang.ClassLoader: " + classLoaderReferences
    )
  }
}


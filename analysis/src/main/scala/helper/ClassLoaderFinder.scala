package helper


import org.opalj.br.Code
import org.opalj.br.analyses.Project

import scala.language.postfixOps

object ClassLoaderFinder {

  private def containsDexCLReference(methodBody: Code): Boolean = {
    val methodBodyText = methodBody toString

    if (methodBodyText contains "dalvik.system.BaseClassLoader") return true
    if (methodBodyText contains "dalvik.system.DexClassLoader") return true
    if (methodBodyText contains "dalvik.system.InMemoryDexClassLoader") return true
    if (methodBodyText contains "dalvik.system.PathClassLoader") return true
    if (methodBodyText contains "dalvik.system.DelegateLastClassLoader") return true

    false
  }

  private def containsNonDexCLReference(methodBody: Code): Boolean = {
    val methodBodyText = methodBody toString

    if (methodBodyText contains "java.lang.ClassLoader") return true
    if (methodBodyText contains "java.net.URLClassLoader") return true

    false
  }

  def analyzeConcreteJAR(projectJARPath: String): Unit = {

    val p =
      Project(new java.io.File(projectJARPath), org.opalj.bytecode.RTJar)
    var numReflectionCalls: Int = 0

    var numberOfDexClassLoaderReferences: Int = 0

    var numberOfNonDexClassLoaderReferences: Int = 0

    p.allMethodsWithBody.foreach { method => {
      val body = method.body
      if (body isDefined) {
        if (body.toString.contains("java.lang.reflect")) {
          numReflectionCalls += 1
        }
        if (containsDexCLReference(body.get)) {
          numberOfDexClassLoaderReferences += 1

        }
        if (containsNonDexCLReference(body.get)) {
          numberOfNonDexClassLoaderReferences += 1

          /*
          body.get.foreach {
            pcAndInstruction => println("InstructionPrinted" + pcAndInstruction.instruction.toString(pcAndInstruction.pc))
          } // Output e.g. InstructionPrintedINVOKESPECIAL(androidx.fragment.app.Fragment$SavedState{ void <init>(android.os.Parcel,java.lang.ClassLoader) })
          */

          val methodInvokationInstructions = body.get.instructions.filter {
            instruction =>
              instruction != null && instruction.isMethodInvocationInstruction
          }
          val objectTypes = methodInvokationInstructions.filter {
            instruction =>
              instruction.asMethodInvocationInstruction.declaringClass.isObjectType
          }
          val classLoaderUsages = objectTypes.filter { instruction =>
            instruction.asMethodInvocationInstruction.declaringClass.asObjectType.fqn
              .contains("java/lang/ClassLoader")
          }
          if (classLoaderUsages.nonEmpty) {
            println(
              method.classFile.fqn + " => " + method.toJava + " => " + classLoaderUsages.head
            )
          }
        }

      }

    }
    }

    println(
      "Found " + numReflectionCalls + " classes that may contain reflections and " +
        numberOfDexClassLoaderReferences + " classes containing dexClassLoader references"
    )
    println(
      "Dexclassloader: " + numberOfDexClassLoaderReferences + " java.lang.ClassLoader: " + numberOfNonDexClassLoaderReferences
    )
  }
}


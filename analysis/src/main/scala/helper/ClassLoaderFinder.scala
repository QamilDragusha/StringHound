package helper


import org.opalj.br.{Code, Method, ObjectType, ObjectTypes, PCInMethod}
import org.opalj.br.analyses.Project
import org.opalj.br.instructions.{INVOKESPECIAL, Instruction}
import org.opalj.collection.immutable.ConstArray
import org.opalj.slicing.DeobfuscationSlicer

import java.net.URL
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

class ClassLoaderFinder(project: Project[URL]) {


  // QAMIL: TODO: Check if that even needs to be implemented
  def findClassLoaderPathStrings() : Map[String, ConstArray[PCInMethod]] = {
     Map()
  }

  private def instructionContainsClassLoaderOfInterest() (implicit instructionText: String) : Boolean = {
    if (instructionText contains "dalvik.system.BaseClassLoader") return true
    if (instructionText contains "dalvik.system.DexClassLoader") return true // QAMIL: Sollte das nicht BaseDexClassLoader sein?
    if (instructionText contains "dalvik.system.InMemoryDexClassLoader") return true
    if (instructionText contains "dalvik.system.PathClassLoader") return true
    if (instructionText contains "dalvik.system.DelegateLastClassLoader") return true
    false
  }

  private def instructionInvokesClassLoaderOfInterest() (implicit instruction: Instruction) : Boolean = {
    implicit val instructionText = instruction.toString()
    instruction.opcode == INVOKESPECIAL.opcode && instructionContainsClassLoaderOfInterest() && instructionText.contains("<init>")
  }

  private def findClassifiedClassLoaderReferenceMethods() : List[(Int, Method)] = {


    var foundCases : List[(Int, Method)] = Nil

    project.allProjectClassFiles foreach {
      classFile => {
        classFile.methodsWithBody foreach {
          methodWithBody => {
            val methodBody : Code = methodWithBody.body.get

            methodBody foreach {
              pcAndInstruction => {
                implicit val instruction = pcAndInstruction.instruction
                if (instructionInvokesClassLoaderOfInterest()) {
                  foundCases = (pcAndInstruction.pc, methodWithBody) :: foundCases
                }
              }
            }

          }
        }
      }
    }

    foundCases
  }

  def findCLassLoaderReferenceMethods(): List[(Int, Method)] = {

    return findClassifiedClassLoaderReferenceMethods()

    var methodsCreatingClassLoaders : List[(Int, Method)] = Nil;

    var matchedMethodsCreatingClassLoaders : List[(Int, Method)] = Nil

    val classesInheritingCLs = project.allProjectClassFiles filter {
      // TODO: Werden hier Parameter-Positionen verändert?
      // Im INVOKESPECIAL pattern-matching dann dementsprechend berücksichtigen (buildMethidForOrigin)
      projectClassFile => {
        val superClasses = project.classHierarchy.allSuperinterfacetypes(projectClassFile.thisType, false)
        superClasses.exists{
          superClass => (superClass == ObjectType("java/lang/ClassLoader") || superClass == ObjectType("dalvik/system/BaseClassLoader")  || superClass== ObjectType("dalvik/system/DexClassLoader")
            || superClass == ObjectType("dalvik/system/InMemoryDexClassLoader") || superClass == ObjectType("dalvik/system/PathClassLoader")
            || superClass == ObjectType("dalvik/system/DelegateLastClassLoader"))
        }
    }
    }

    classesInheritingCLs foreach {
      file => println(file)
    }

    val objectTypes = classesInheritingCLs.map {
      classFile => classFile.thisType
    }


    project.allMethodsWithBody foreach { method => {
      val body = method.body.get

      body.foreach {
        pcAndInstruction => {
          (pcAndInstruction.pc, pcAndInstruction.instruction) match {
            case (pc, INVOKESPECIAL(declaringClass, _, name,_ )) => {
              if (objectTypes.contains(declaringClass) && name.equals("<init>")) {
                methodsCreatingClassLoaders = (pc, method) :: methodsCreatingClassLoaders
              }
            }
            case _  =>
          }
        }
      }
    }
    }

    methodsCreatingClassLoaders
  }
}


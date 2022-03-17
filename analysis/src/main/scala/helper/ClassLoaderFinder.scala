package helper

import org.opalj.br.{ClassFile, Code, Method, ObjectType, ObjectTypes, PC, PCAndInstruction, PCInMethod}
import org.opalj.br.analyses.Project
import org.opalj.br.instructions.{INVOKESPECIAL, Instruction}
import org.opalj.collection.immutable.ConstArray

import java.net.URL
import scala.collection.mutable
import scala.language.postfixOps

class ClassLoaderFinder(project: Project[URL]) {

  private val standardClassLoaderTypes = ConstArray(
    "java/lang/ClassLoader",
    "dalvik/system/BaseClassLoader",
    "dalvik/system/DexClassLoader",
    "dalvik/system/InMemoryDexClassLoader",
    "dalvik/system/PathClassLoader",
    "dalvik/system/DelegateLastClassLoader"
  ) map {ObjectType(_)}

  // TODO: Werden hier Parameter-Positionen verändert?
  // Im INVOKESPECIAL pattern-matching dann dementsprechend berücksichtigen (buildMethidForOrigin)
  lazy private val customClassLoaderClassFiles =
  project.allProjectClassFiles filter classFileInheritsClassLoader

  lazy private val customClassLoaderTypes = customClassLoaderClassFiles.map(_.thisType)

  // This contains all (not dynamically loaded) classLoaders the project could use
  // so we can easily find out most places classes could be loaded
  implicit lazy private val projectWideClassLoaderTypes: ConstArray[ObjectType] = {
  standardClassLoaderTypes ++ customClassLoaderTypes
  }

  private val foundInstanciationPoints : mutable.HashMap[Int, mutable.HashSet[Method]] = mutable.HashMap()

  private var instantionPointsHaveBeenSet = false

  /** Finds all Points (meaning PCs in Methods) in the project where a directly defined ClassLoader is instanciated */
  def findClassLoaderInstantiatingMethodPoints(): ConstArray[(Int, Method)] = {
    val foundInstantiations = project.allMethodsWithBody flatMap findClassLoaderInstantiatingMethodInstructions
    // enables easy and fast look-up when checking if a given instruction performs an invocation of a known ClassLoader
    saveFoundInstantiationPoints(foundInstantiations)
    foundInstantiations
  }

  /** Quickly checks, whether the given point in a method is known to instantiate a ClassLoader */
  def instructionInstantiatesKnownClassLoader(pc: Int, method: Method) : Boolean = {
    assert(instantionPointsHaveBeenSet)
    // Is faster than to re-check a method since we already have the hashmap
    if (!foundInstanciationPoints.contains(pc)) {
      return false
    }
    foundInstanciationPoints(pc).contains(method)
  }

  private def classFileInheritsClassLoader(classFile: ClassFile): Boolean = {
    val superClasses = project.classHierarchy
      .allSupertypes(classFile.thisType, false)

    superClasses.exists(standardClassLoaderTypes.contains)
  }

  private def saveFoundInstantiationPoints(foundInstantiations: ConstArray[(Int, Method)]) : Unit = {
    foundInstantiations foreach { instantiationPoint =>
      val pc = instantiationPoint._1
      val method = instantiationPoint._2
      if (!foundInstanciationPoints.contains(pc)) {
        foundInstanciationPoints += (pc -> mutable.HashSet(method))
      } else {
        foundInstanciationPoints(pc) += method
      }
      // This is a workaround around bug resolving around array loops. See https://github.com/scala/bug/issues/10151
      Unit
    }
    instantionPointsHaveBeenSet = true
  }

  private def findClassLoaderInstantiatingMethodInstructions(method: Method)(
    implicit projectWideClassLoaderTypes: ConstArray[ObjectType]
  ): Set[(Int, Method)] = {
    val methodBody = method.body.get
    methodBody filter {(_, instruction) => methodInstructionInstantiatesClassLoader(instruction)} map {pc => (pc, method)}
  }

  private def methodInstructionInstantiatesClassLoader(
      instruction: Instruction,
  )(implicit projectWideClassLoaderTypes: ConstArray[ObjectType]): Boolean = {
     instruction match {
      case INVOKESPECIAL(declaringClass, _, name, _) => {
        if (
          projectWideClassLoaderTypes
            .contains(declaringClass) && name.equals("<init>")
        ) { return true }
        false
      }
      case _ => false
    }
  }


}

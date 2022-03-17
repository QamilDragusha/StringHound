package helper

import org.opalj.br.{ClassFile, Method, ObjectType}
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
  ) map { ObjectType(_) }

  // TODO: Werden hier Parameter-Positionen verändert?
  // Im INVOKESPECIAL pattern-matching dann dementsprechend berücksichtigen (buildMethidForOrigin)
  lazy private val customClassLoaderClassFiles =
    project.allProjectClassFiles filter classFileInheritsClassLoader

  lazy private val customClassLoaderTypes =
    customClassLoaderClassFiles.map(_.thisType)

  // This contains all (not dynamically loaded) classLoaders the project could use
  // so we can easily find out most places classes could be loaded
  implicit lazy private val projectWideClassLoaderTypes
      : ConstArray[ObjectType] = {
    standardClassLoaderTypes ++ customClassLoaderTypes
  }

  private val foundInstanciationPoints
      : mutable.HashMap[Int, mutable.HashSet[Method]] = mutable.HashMap()
  private val foundClassLoadingPoints
      : mutable.HashMap[Int, mutable.HashSet[Method]] = mutable.HashMap()

  private var instantionPointsHaveBeenSet = false
  private var classLoadingPointsHaveBeenSet = false

  /** Finds all Points (meaning PCs in Methods) in the project where a directly defined ClassLoader is instantiated */
  def findClassLoaderInstantiationMethodPoints(): ConstArray[(Int, Method)] = {
    val foundInstantiations =
      project.allMethodsWithBody flatMap findClassLoaderInstantiationInstructions
    // enables easy and fast look-up when checking if a given instruction performs an invocation of a known ClassLoader
    saveFoundInstantiationPoints(foundInstantiations)
    foundInstantiations
  }

  /** Quickly checks, whether the given point in a method is known to instantiate a ClassLoader */
  def instructionInstantiatesKnownClassLoader(
      pc: Int,
      method: Method
  ): Boolean = {
    assert(instantionPointsHaveBeenSet)
    // Is faster than to re-check a method since we already have the hashmap
    if (!foundInstanciationPoints.contains(pc)) {
      return false
    }
    foundInstanciationPoints(pc).contains(method)
  }

  /** Finds Points (meaning PCs in Methods) in the project where any ClassLoader's loadClass is invoked */
  def findClassLoadingMethodPoints(): ConstArray[(Int, Method)] = {
    project.allMethodsWithBody flatMap findClassLoadingInstructions
  }

  /** Quickly checks, whether the given point in a method is known to load a class */
  def instructionIsKnownToLoadAClass(pc: Int, method: Method): Boolean = {
    assert(classLoadingPointsHaveBeenSet)
    if (!foundClassLoadingPoints.contains(pc)) {
      return false
    }
    foundClassLoadingPoints(pc).contains(method)
  }

  private def classFileInheritsClassLoader(classFile: ClassFile): Boolean = {
    val superClasses = project.classHierarchy
      .allSupertypes(classFile.thisType, false)

    superClasses.exists(standardClassLoaderTypes.contains)
  }

  private def findClassLoaderInstantiationInstructions(method: Method)(implicit
      projectWideClassLoaderTypes: ConstArray[ObjectType]
  ): Set[(Int, Method)] = {
    val methodBody = method.body.get
    methodBody filter { (_, instruction) =>
      instructionInvokesClassLoaderMethodWithName(instruction, "<init>")
    } map { pc => (pc, method) }
  }

  private def findClassLoadingInstructions(method: Method)(implicit
      projectWideClassLoaderTypes: ConstArray[ObjectType]
  ): Set[(Int, Method)] = {
    val methodBody = method.body.get
    methodBody filter { (_, instruction) =>
      instructionInvokesClassLoaderMethodWithName(instruction, "loadClass")
    } map { pc => (pc, method) }
  }

  /** Returns whether a method with a given [methodName] is invoked on any known ClassLoader in the project */
  private def instructionInvokesClassLoaderMethodWithName(
      instruction: Instruction,
      methodName: String
  )(implicit projectWideClassLoaderTypes: ConstArray[ObjectType]): Boolean = {
    instruction match {
      case INVOKESPECIAL(declaringClass, _, name, _) => {
        if (
          projectWideClassLoaderTypes
            .contains(declaringClass) && name.equals(methodName)
        ) { return true }
        false
      }
      case _ => false
    }
  }

  private def saveFoundInstantiationPoints(
      foundInstantiations: ConstArray[(Int, Method)]
  ): Unit = {
    saveMethodPoints(foundInstantiations, foundInstanciationPoints)
    instantionPointsHaveBeenSet = true
  }

  private def saveFoundClassLoadingPoints(
      foundClassLoadings: ConstArray[(Int, Method)]
  ): Unit = {
    saveMethodPoints(foundClassLoadings, foundClassLoadingPoints)
    classLoadingPointsHaveBeenSet = true
  }

  private def saveMethodPoints(
      foundInstantiations: ConstArray[(Int, Method)],
      storage: mutable.HashMap[Int, mutable.HashSet[Method]]
  ): Unit = {
    foundInstantiations foreach { instantiationPoint =>
      val pc = instantiationPoint._1
      val method = instantiationPoint._2
      if (!storage.contains(pc)) {
        storage += (pc -> mutable.HashSet(method))
      } else {
        storage(pc) += method
      }
      // This is a workaround around bug resolving around array loops. See https://github.com/scala/bug/issues/10151
      Unit
    }
  }

}

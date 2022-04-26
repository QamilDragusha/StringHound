package helper

import org.opalj.br.{ClassFile, Method, ObjectType}
import org.opalj.br.analyses.Project
import org.opalj.br.instructions.{INVOKESPECIAL, INVOKEVIRTUAL, Instruction}
import org.opalj.collection.immutable.ConstArray

import java.net.URL
import scala.collection.mutable
import scala.collection.immutable
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

  private var instantiatedClassLoaderTypes: Set[ObjectType] = Set()

  def findClassLoaderInstantiationsAndVariety()
      : (mutable.HashMap[Method, Array[Int]], Set[ObjectType]) = {
    instantiatedClassLoaderTypes = Set()
    (findClassLoaderInstantiations(logVariety = true), instantiatedClassLoaderTypes)
  }


  def findClassLoaderInstantiations(
      logVariety: Boolean = false
  ): mutable.HashMap[Method, Array[Int]] = {
    val foundInstatiations: mutable.HashMap[Method, Array[Int]] =
      mutable.HashMap()
    project.allMethodsWithBody foreach { method =>
      {
        val classLoaderInstantiationInstructions =
          findClassLoaderInstantiationInstructions(method, logVariety)
        if (!classLoaderInstantiationInstructions.isEmpty) {
          foundInstatiations += (method -> classLoaderInstantiationInstructions)
        }
      }

    }
    //foundInstatiations foreach println
    foundInstatiations
  }

  def findClassLoaderUsages(): mutable.HashMap[Method, Array[Int]] = {
    val foundUsages: mutable.HashMap[Method, Array[Int]] = mutable.HashMap()
    project.allMethodsWithBody foreach { method =>
      {
        val classLoaderUsageInstructions = findClassLoadingInstructions(method)
        if (!classLoaderUsageInstructions.isEmpty) {
          foundUsages += (method -> findClassLoadingInstructions(method))
        }

      }
    }
    foundUsages
  }

  private def classFileInheritsClassLoader(classFile: ClassFile): Boolean = {
    val superClasses = project.classHierarchy
      .allSupertypes(classFile.thisType, false)

    superClasses.exists(standardClassLoaderTypes.contains)
  }

  private def findClassLoaderInstantiationInstructions(
      method: Method,
      logVariety: Boolean
  )(implicit
      projectWideClassLoaderTypes: ConstArray[ObjectType]
  ): Array[Int] = {
    val methodBody = method.body.get
    val invocationLocations = methodBody filter { (_, instruction) =>
      instructionInvokesClassLoaderMethodWithName(
        instruction,
        "<init>",
        logVariety
      )
    }

    invocationLocations.toArray
  }

  private def findClassLoadingInstructions(method: Method)(implicit
      projectWideClassLoaderTypes: ConstArray[ObjectType]
  ): Array[Int] = {
    val methodBody = method.body.get
    val loadClassLocations = methodBody filter { (_, instruction) =>
      instructionInvokesClassLoaderMethodWithName(instruction, "loadClass", false)
    }

    loadClassLocations.toArray
  }

  /** Returns whether a method with a given [methodName] is invoked on any known ClassLoader in the project */
  private def instructionInvokesClassLoaderMethodWithName(
      instruction: Instruction,
      methodName: String,
      logVariety: Boolean
  )(implicit projectWideClassLoaderTypes: ConstArray[ObjectType]): Boolean = {
    instruction match {
      case INVOKESPECIAL(declaringClass, _, name, _) => {
        if (
          projectWideClassLoaderTypes
            .contains(declaringClass) && name.equals(methodName)
        ) {
          if (logVariety)
            instantiatedClassLoaderTypes ++= Set(declaringClass.asObjectType)
          return true
        }
        false
      }
      case INVOKEVIRTUAL(declaringClass, name, _) => {
        if (
          projectWideClassLoaderTypes
            .contains(declaringClass) && name.equals(methodName)
        ) { return true }
        false
      }
      case _ => false
    }
  }

}

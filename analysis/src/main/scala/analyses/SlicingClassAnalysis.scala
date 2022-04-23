package analyses

import java.io._
import java.net.URL
import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.concurrent.atomic.AtomicInteger
import java.util.{Timer, TimerTask}
import customBRClasses.leakers.{ByteBufferLeaker, StringLeaker}
import debugging.Dumper
import helper.{APKManager, AndroidLibraryMocker, ClassLoaderFinder}
import main.StringDecryption
import org.apache.commons.lang3.ClassUtils
import org.mockito.Mockito.{mock, when, withSettings}
import org.mockito.internal.stubbing.answers.ThrowsException
import org.opalj.{Answer => _, _}
import org.opalj.ai.domain.PerformAI
import org.opalj.ai.domain.l1.DefaultDomainWithCFGAndDefUse
import org.opalj.ai.{AIResult, ValueOrigin}
import org.opalj.ba.{CLASS, CODE, CodeAttributeBuilder, CodeElement, FIELD, FIELDS, METHOD, METHODS, PUBLIC, STATIC, toDA}
import org.opalj.bc.Assembler
import org.opalj.bi.ACC_PUBLIC
import org.opalj.br.analyses.{Project, StringConstantsInformationKey}
import org.opalj.br.instructions._
import org.opalj.br.{PCInMethod, _}
import org.opalj.collection.immutable.{ConstArray, RefArray}
import org.opalj.slicing.{ClassDeobfuscationSlicer, DeobfuscationSlicer, ParameterUsageException, SlicingConfiguration}
import org.opalj.util.InMemoryAndURLClassLoader
import android.content.Context
import android.content.pm.{ApplicationInfo, PackageInfo, PackageManager}
import android.content.res.{AssetManager, Resources}
import android.os.Bundle
import customBRClasses.dummy.DummyClass
import models.ClassSlicingContext
import org.mockito.ArgumentMatchers.{anyInt, anyString}
import org.mockito.invocation.InvocationOnMock
import org.mockito.mock.MockCreationSettings
import org.mockito.stubbing.Answer

import java.nio.ByteBuffer
import scala.io.Source
import scala.reflect.runtime.universe.typeOf

class SlicingClassAnalysis(
    val pathToApk: String,
    val parameters: Seq[String]
) {

  private val apkManager = new APKManager(pathToApk)
  private val project = apkManager.opalProject


  implicit val projectToAnalyze: Project[URL] = project
  implicit val classHierarchy: ClassHierarchy = project.classHierarchy

  private lazy val charSeqClazz = Class.forName("java.lang.CharSequence")
  private val CharSequenceObjectType = ObjectType("java/lang/CharSequence")

  var genericErrors = new AtomicInteger
  var out: PrintStream = _
  var err: PrintStream = _
  val devNullPrintStream = new DevNullPrintStream()
  var attempts = new AtomicInteger
  var executions = new AtomicInteger
  var verifyErrors = new AtomicInteger
  var timeouts = new AtomicInteger
  var methodCalls = new AtomicInteger
  var successful = new AtomicInteger
  var invocationTargetError = new AtomicInteger
  var parameterErrors = new AtomicInteger
  var usesParams = new AtomicInteger
  var noClassDef = new AtomicInteger
  var otherSource = new AtomicInteger
  var nullPointerError = new AtomicInteger
  var resultStream: FileWriter = _
  var urls: Array[URL] = _
  var libraryClasses: Set[ClassFile] = Set()
  var constantStrings = Set.empty[String]
  var stringUsages = List.empty[String]

  var debug = false

  var allThreads = Set.empty[Long]
  var first = true
  var clean = false

  var bruteforce = false

  val logStream = new FileWriter(
    new File(
      StringDecryption.outputDir + "/logs/" + parameters.head + "Log.txt"
    ),
    false
  )

  val cleanTask: TimerTask = new TimerTask() {
    override def run(): Unit = {
      if (!debug) {
        // TODO: QAMIL: Das muss wieder rückgängig gemacht werden
         clean = true
      }
    }
  }

  val slicingTimerTask: TimerTask = new TimerTask() {
    override def run(): Unit = {
      val slicesCount = attempts.get()
      System.setOut(out)
      println("#slices: " + slicesCount)
      System.setOut(devNullPrintStream)
    }
  }

  val executionTimerTask: TimerTask = new TimerTask() {
    override def run(): Unit = {
      val executionCount = executions.get()
      System.setOut(out)
      println("#executions: " + executionCount)
      System.setOut(devNullPrintStream)
    }
  }

  val relevantSinks: Set[String] = {
    val relevantSinksSource = Source
      .fromFile("relevantSinks.txt")

    val relevantSinks = relevantSinksSource.getLines().
      filter(l => l.nonEmpty && l.startsWith("<"))
      .map { l =>
        val split = l.substring(1, l.lastIndexOf('>')).split(": ")
        val fqn = split(0)
        val methodSignature = MethodSignatureWrapper(split(1)).toString
        fqn + " " + methodSignature
      }
      .toSet

    relevantSinksSource.close()

    relevantSinks
  }

  var decryptionContextSet =
    Map.empty[Method, (MethodTemplate, Set[ClassFile], ClassFile)]

  def doAnalyze(
      t0: Long,
      bf: Boolean,
      debug: Boolean,
      isAndroid: Boolean
  ): Unit = {
    setupAnalysis(bf, debug, isAndroid)

    val start = Instant.now()

    val t1, t2, t3 = System.currentTimeMillis()

    val classLoaderFinder = new ClassLoaderFinder(project)

    val classLoaderInstatiations =
      classLoaderFinder.findClassLoaderInstantiations()

    val methodInstructionsToAnalyze =
      classLoaderInstatiations. // TODO: Qamil: Nut für Debugging-Zwecke.
        filter{ clIn => clIn._1.classFile.fqn.contains("zzfy") }

    println("printed methodInstructionsInstatiatingClassLoaders")

    if (methodInstructionsToAnalyze.nonEmpty) {

      System.setOut(out)
      println("Methods to slice: " + methodInstructionsToAnalyze.size)
      System.setOut(devNullPrintStream)

      val cleanTimer = new Timer()
      cleanTimer.schedule(cleanTask, 1000, 120000)
      val timer = new Timer()
      timer.schedule(slicingTimerTask, 1000, 10000)
      methodInstructionsToAnalyze.foreach { methodPcPair =>
        {
          val (method, programCounters) = methodPcPair

          // QAMIL: DUMP METHOD ########
          Dumper.dumpMethod(method, "doAnalyze/")

          val methodBody = method.body.get

          try {

            val domain =
              new ai.domain.l1.DefaultDomainWithCFGAndDefUse(project, method)
                with SlicingConfiguration

            lazy val result
                : AIResult { val domain: DefaultDomainWithCFGAndDefUse[URL] } =
              PerformAI(domain)

            programCounters.foreach { pc =>
              val instruction = methodBody.instructions(pc)

              instruction.opcode match {
                // Einziger relevante Fall
                case INVOKESPECIAL.opcode =>
                  //println("invokeSpecial matched case")
                  val invoke = instruction.asMethodInvocationInstruction
                  val params = invoke.methodDescriptor.parameterTypes
                  val sig = invoke.methodDescriptor.toJava(invoke.name)


                  params.iterator.zipWithIndex foreach { parameter =>
                    val (parameterType, index) = parameter
                    // println("ParamaterType: " + parameterType+ " , instruction: "+ instruction)
                    if (
                      typeIsOrHoldsCharSequenceObjectType(
                        parameterType
                      ) || parameterType == ObjectType.Object ||
                      // qamil TODO: In diesem Fall muss das so vermerkt wrerden, dass der Slicer das dann berücksichtigen kann und den richtigen Leaker einbaut
                      parameterType == ObjectType(
                        "java/nio/ByteBuffer"
                      )
                    ) {
                      println("\n\n\nmatching instruction " + instruction + ", declared at " + instruction.asMethodInvocationInstruction.declaringClass)
                      println("typeChecked: " + parameterType)

                      // QAMIL: DUMP ANOTHER METHOD ########
                      Dumper.dumpMethod(
                        method,
                        "iterateDoAnalyze/",
                        "Instruction at pc (" + pc + ") with type of Interest " + parameterType + " will be processed at " + (params.size - 1 - index) + "\n Instruction: \n" + instruction
                      )

                      val classSlicingContext = ClassSlicingContext.fromConstructorInvocation(instruction.asMethodInvocationInstruction, parameterType)

                      // The context may not be defined in cases where the parameters are irrelevant
                      if (classSlicingContext.isDefined) {
                        processOrigins(
                          params.size - 1 -  index,
                          new SinkInfo(invoke.declaringClass, sig, pc),
                          method,
                          project,
                          classSlicingContext.get,
                          result
                        )
                      }


                    }
                  }

                case _ => // Rest
              }

            }
          } catch {
            case e: Throwable ⇒
              println(s"doAnalyze catched exception $e")
              if (StringDecryption.logSlicing) {
                val sb = new StringBuilder()
                sb.append(parameters.head + "\n")
                  .append(method + "\n")
                  .append(method.classFile + "\n")
                StringDecryption.logger.error(sb.toString())
                StringDecryption.logger.error(e.getStackTrace.mkString("\n"))
              }
          }
        }

      }
      timer.cancel()
      cleanTimer.cancel()

    } else {
      System.setOut(out)
      println("No encryption method skipping: " + parameters.head)

    }
    val t4 = System.currentTimeMillis()
    System.setErr(err)
    System.setOut(out)
    println(
      "Write results to -> " + StringDecryption.outputDir + "/results/" + parameters.head + ".txt"
    )

    teardownAnalysis(t0, t1, t2, t3, start)
  }

  def setupAnalysis(bf: Boolean, debug: Boolean, isAndroid: Boolean): Unit = {
    out = System.out
    err = System.err
    bruteforce = bf
    this.debug = debug
    libraryClasses = project.allLibraryClassFiles.toSet
    if (isAndroid)
      urls = List.empty[URL].toArray
    else
      urls = new File(parameters.tail.head)
        .listFiles(f ⇒ f.getName.endsWith(".jar"))
        .map(_.toURI.toURL)
    resultStream = new FileWriter(
      new File(
        StringDecryption.outputDir + "/results/" + parameters.head + ".txt"
      ),
      false
    )

    logStream.write(
      "Apk;PreAnalysisTime;StringClassifierTime;MethodClassifierTime;SlicingTime;OverallTime;ClassCount;MethodCount;MeanInstPerMethodCount;MedianInstPerMethodCount;MaxInstPerMethodCount;ApkInstCount;StringUniqCount;DecryptedStrings;SlicesCount\n"
    )
    System.setOut(devNullPrintStream)
    System.setErr(devNullPrintStream)
  }

  def isRelevantSink(mii: MethodInvocationInstruction): Boolean = {
    if (mii.declaringClass.isObjectType) {
      val methodSignature = MethodSignatureWrapper(
        mii.methodDescriptor.toJava(mii.name)
      ).toString
      val sink = mii.declaringClass.asObjectType.fqn
        .replace('/', '.') + " " + methodSignature
      relevantSinks.contains(sink)
    } else false
  }

  def findMethodInvokations(
      methodsOfInterest: ConstArray[(Int, Method)]
  ): Set[Method] = {
    var methodsInvokingMethodsOfInterest: Set[Method] = Set.empty[Method]
    try {
      val callGraph = CallGraphFactory.createOPALCallGraph(project)
      methodsInvokingMethodsOfInterest = methodsOfInterest
        .flatMap(cm =>
          callGraph(cm._2).map(f => f._1.asDefinedMethod.definedMethod).toSet
        )
        .toSet
    } catch {
      case _: Throwable =>
        println("Throwable without print")
        // qamil: Brauchen wir das mit dem CallGraphen überhaupt?
        val callGraph = CallGraphFactory.createCHACallGraph(project)
        methodsInvokingMethodsOfInterest = methodsOfInterest
          .flatMap(cm =>
            if (callGraph.contains(cm._2)) callGraph(cm._2)
            else Set.empty[Method]
          )
          .toSet
    }

    methodsInvokingMethodsOfInterest
  }

  /// Index des Parameters kann buer schon zur Deobfuskation genbutzt werden
  def processOrigins(
      index: Int,
      sinkInfo: SinkInfo,
      method: Method,
      project: Project[URL],
      context: ClassSlicingContext,
      result: AIResult { val domain: DefaultDomainWithCFGAndDefUse[URL] }
  ): Unit = {

    Dumper.dumpMethod(method, "processOrigins/")

    val operands = result.operandsArray(sinkInfo.sinkPC)
    if (operands == null) return
    //println("after return: index = " + index + ", sinkInfo = " + sinkInfo + ", method = " + method + ", project = " + "project" + ", resultDomain = " + result.domain)
    val op = operands(index)
    val methodClass = method.classFile.fqn
    println(s"matching op with index $index from $methodClass and method $method")
    op match {
      case result.domain.StringValue(
            s
          ) ⇒ println("We are missing a single String ")// Not Obfuscated or deob method // Sollte ich vllt nachverfolgen wenn ein String hierdrinsteht, manche Klassen stehen im Klartext
       println("StringValue" + s)
      // QAMIL : Scheint bei den Methoden hier die Regel zu sein
      case result.domain.DomainReferenceValueTag(v) ⇒
        //println("DOMAINREFERENCEVALUETAG")
        // QAMIL : Was ist v und p ?
        if (
          v.allValues
            .exists(p => p.upperTypeBound.containsId(ObjectType.String.id))
        ) {

          result.domain
            .originsIterator(op)
            .foreach(
              buildMethodForOrigin(
                _,
                sinkInfo,
                project,
                method: Method,
                context,
                result
              )
            )
        }
        // TODO: Nochmal schauen, ob das anders gehandelt werden muss
        else if (
          v.allValues
            .exists(p =>
              p.upperTypeBound.containsId(ObjectType("java/nio/ByteBuffer").id)
            )
        ) {
          result.domain
            .originsIterator(op)
            .foreach(
              buildMethodForOrigin(
                _,
                sinkInfo,
                project,
                method: Method,
                context,
                result
              )
            )
        }

        else {
          val types = v.allValues.mkString(" - ")
          println(s"No type of interest was found, may be null. Types: $types ")
        }

      case result.domain.MultipleReferenceValues(s) ⇒
        s.foreach {

          case result.domain.StringValue(st) ⇒ println("Here is a String we have ignored") //println("DOMAINMULTIPLEREFERENCEVALUES") // Not Obfuscated or deob method
          case value ⇒
            //println("DOMAINMULTIPLEREFERENCEVALUES")
            println("V: " + value.allValues.mkString(" - "))
            value.origins.foreach(
              buildMethodForOrigin(
                _,
                sinkInfo,
                project,
                method: Method,
                context,
                result
              )
            )
        }
      case e ⇒
        print("processOrigins wont process " + e)
    }
    //                    domain.foreachOrigin(op, buildMethodForOrigin(_))
  }

  // Noichmal schauen, on das Gesuchte hier gematched wird
  // Origin relevatn für Instanzmethoden (es wird this übergeben), checken, ob es sich um die Instanz oder eigentliche Parameter handelt

  //Bei mir sollte die Origin der Index des Parameters sein, welcher den Pfad/Bytecode

  //Was sollte reinkommen: Bei welcher Methode wird welcher Parameter aufgerufen -> Parameter ist origin
  def buildMethodForOrigin(
      origin: Opcode,
      sinkInfo: SinkInfo,
      project: Project[URL],
      method: Method,
      context: ClassSlicingContext,
      result: AIResult { val domain: DefaultDomainWithCFGAndDefUse[URL] }
  ): Unit = {
    // QAMIL : macht Opcode bei origin überhaupt Sinn? Sieht nämlich eher so aus, als sei das die
    // Nummer der Instruktion im Code, also ein Identifier für die richtige Instruktion
    // Parameter sind negativ, hier wird versucht, keine Parameter weiter zu slicen, da intraprozedual
    println("origin : " + origin)

    // QAMIL: Dump both variations ######
    Dumper.dumpMethod(
      method,
      "buildMethodForOrigin/",
      "Origin at pc (" + origin + ")\nInstruction:\n" + result.code.instructions(
        origin
      )
    )
    Dumper.dumpCode(
      result.code,
      method.classFile.fqn + "->" + method.name + "origins",
      "buildMethodForOrigin/code/",
      "Origin at pc (" + origin + ")"
    )
    if (origin >= 0) { // Qamil 21.3: Origin ist also die konkrete Instruktion
      try {
        val ins: Instruction = result.code.instructions(origin)
        println("Built-target instruction: " + ins)
        ins.opcode match {
          // QAMIL: War vorher aus irgendeinem Grund nur Invokespeacial
          case INVOKESPECIAL.opcode | INVOKEVIRTUAL.opcode |
              INVOKESTATIC.opcode ⇒
            // HIER DEN STRING / BYTEARRAY nachverfolgen, welcher dem ClassLoader übergeben wird

            println("MATCHED Invocation")

            val mii = ins.asMethodInvocationInstruction
            val hasBaseOrStringParam =
              mii.methodDescriptor.parameterTypes.exists { fieldType ⇒
                val isBaseOrStringArrayType = if (fieldType.isArrayType) {
                  val elementType = fieldType.asArrayType.elementType
                  elementType.isBaseType || elementType.isObjectType &&
                  (elementType == ObjectType.Object || typeIsOrHoldsCharSequenceObjectType(
                    elementType
                  )(
                    project.classHierarchy
                  ))
                } else {
                  false
                }
                fieldType.isBaseType ||
                fieldType == ObjectType.Object ||
                isBaseOrStringArrayType ||
                (fieldType.isObjectType && typeIsOrHoldsCharSequenceObjectType(
                  fieldType
                )(
                  project.classHierarchy
                ))
              }
            val isJDKMethod =
              mii.declaringClass.isObjectType &&
                project.isLibraryType(mii.declaringClass.asObjectType)

            // Qamil: TODO: Wird lamgsam überflüssig mit der typeOfInterest-basierten Architektur, nochmal beurteilen
            val isToStringOrOnString = mii.name == "toString" ||
              typeIsOrHoldsCharSequenceObjectType(mii.declaringClass)(
                project.classHierarchy
              )

            // origin muss die Instruktion sein, die in den Parameter einfließt, der byteCode oder Pfad ist => SlicingCriterion
            // Herausfinden, welcher Index es ist
            // LoI: Ist die Instanziierung
            // ParameterIndex ist SlicingCriterion
            // QAMIL: Moved that out of the condition
            buildAndCallMethod(
              method,
              origin,
              result,
              sinkInfo,
              context
            )(project)

            if (
              hasBaseOrStringParam &&
              (!isJDKMethod || isToStringOrOnString)
            ) {
              // println("buildAndCallMethod called from INVOKE Case")
              //buildAndCallMethod(method, origin, result, sinkInfo)(project)
            }
          // QAMIL: Neu eingefügt, um im Beispiel vom DynamiteModule die String-Ursprünge zurückverfolgen zu können
          case GETSTATIC.opcode =>
            println("MATCHED GETSTATIC")
            buildAndCallMethod(
              method,
              origin,
              result,
              sinkInfo,
              context
            )(project)

          case _ ⇒ println("MATCHED NOTHING")
          //println(body.instructions(origin))
        }
      } catch {
        case ex: Throwable =>
          println("Throwable thrown")
          if (StringDecryption.logSlicing) {
            StringDecryption.logger.error(parameters.head)
            StringDecryption.logger.error(ex.getMessage)
            StringDecryption.logger.error(ex.getStackTrace.mkString("\n"))
          }
      }
    }
  }

  def buildAndCallMethod(
      method: Method,
      origin: ValueOrigin,
      result: AIResult { val domain: DefaultDomainWithCFGAndDefUse[URL] },
      sinkInfo: SinkInfo,
      context: ClassSlicingContext
  )(implicit project: Project[_]): AnyVal = {
    if (first) {
      allThreads = Thread.getAllStackTraces
        .keySet()
        .toArray()
        .map(f => f.asInstanceOf[Thread].getId)
        .toSet
      first = false
    }
    attempts.incrementAndGet()
    val runner = new Thread(() => {
      try {
        val methodClassFile = method.classFile
        // QAMIL: TODO: Was ist die genaue Aufgabe von accociatedMethodString? Nochmal rausfinden und umbenennen
        val (builtMethodTemplate, associatedMethodString) =
          buildMethod(method, origin, context, result, sinkInfo.sinkPC)

        if (builtMethodTemplate.isDefined) {
          val modifiedMethod = builtMethodTemplate.get

          //qamil ##### DUMP
          Dumper.dumpMethodTemplate(builtMethodTemplate.get)
          //                        val supertype = if (modifiedMethod.isStatic) Some(ObjectType("java/lang/Object")) else methodClassFile.superclassType

          val neededInterfaces = findInvokedInterfaceMethodsInSlice(
            modifiedMethod,
            methodClassFile.interfaceTypes
          )

          val modifiedClass = addMethodToClass(methodClassFile, modifiedMethod)
            .copy(
              accessFlags = methodClassFile.accessFlags | ACC_PUBLIC.mask,
              interfaceTypes = neededInterfaces
            )

          val linkedMethod = modifiedClass.methods
            .filter(m =>
              m.name == modifiedMethod.name &&
                m.descriptor == modifiedMethod.descriptor
            )
            .head

          // TODO: Qamil: Checke, ob die relevanten Klassen auch transitiv geladen wird
          val (relevantMethods, relevantFields, relevantClasses) =
            getAccessedFieldsMethodsAndClasses(
              modifiedClass,
              Some(linkedMethod)
            )(modifiedClass, project)

          val superType = /*if (modifiedMethod.name == "<init>")*/ Option(
            DummyClass.objectType
          ) //else modifiedClass.superclassType
          val filteredClass = modifiedClass
            .copy(
              //            fields = modifiedClass.fields.filter(relevantFields.contains(_)).map(_.copy()),
              methods = buildNewRefArray(
                modifiedClass.methods
                  .filter(m =>
                    !m.isAbstract && (relevantMethods
                      .contains(m) || m.name == DUMMYSTATIC)
                  )
                  .toList,
                handleAbstractMethods(modifiedClass)
              ).map[MethodTemplate](_.copy())
            )
            .copy(superclassType = superType)
            .copy(accessFlags = modifiedClass.accessFlags & NON_ABSTRACT)

          val dummyClass = DummyClass.classFile

          val (redirectedClass, newDummy) = handleMissingMethods(
            redirectToDummyClass(
              filteredClass,
              dummyClass,
              modifiedClass.superclassType,
              /*modifiedMethod.name == "<init>"*/ isInitChange = true
            ),
            methodClassFile,
            project
          )

          val newClass =
            handleAllCallsToNewMethod(redirectedClass, modifiedMethod)

          val strippedClasses = (relevantClasses
            .filter(_.fqn != methodClassFile.fqn)
            .map(
              filterMethodsAndFields(_, relevantMethods, relevantFields)
            ) + newDummy + newClass + StringLeaker.classFile + ByteBufferLeaker.classFile // + AndroidContext.classFile
            // qamil: TODO: Optimierung möglich, man muss nicht immer jeden Leaker laden
            // + AndroidContext.classFile + AndroidAssetManager.classFile + CBRByteArrayOutputStream.classFile + CBRByteBuffer.classFile + CBRInputStream.classFile + JavaClassLoader.classFile
            ).
          //map(methodClassFile ⇒ removeAndroidSystemCallsFromStaticInit(methodClassFile)).
          map(classFile ⇒ classFile.copy(version = bi.Java5Version)).map(cf =>
            fixTime(cf)
          )
          //map(methodClassFile ⇒ methodClassFile.copy(methods = methodClassFile.methods.map[MethodTemplate](m ⇒
          //  addStackMapTable(m, project.classHierarchy))))
          val mappedClasses = strippedClasses
            .map(classFile =>
              (classFile.thisType.toJava, Assembler(toDA(classFile)))
            )
            .toMap
          //                        val classLoader = new InMemoryClassLoader(mappedClasses, this.getClass.getClassLoader)

          val classLoader = new InMemoryAndURLClassLoader(
            mappedClasses,
            this.getClass.getClassLoader,
            urls // qamil: URLs seem to be empty most of the time
          )

          strippedClasses
            .filter(c =>
              c.fqn != methodClassFile.fqn && c.fqn != "slicing.StringLeaker" && c.fqn != "slicing.ByteBufferLeaker"
            //&& c.fqn != "android.content.Context" && c.fqn != "android/content/Context"
            )
            .foreach(rcf => {
              //println("Loading class " + rcf.fqn)
              classLoader.loadClass(rcf.thisType.toJava)
            })

          // QAMIL: NOCHMAL DIE (GESLICEDTE) METHOD VOR DER AUSFÜHRUNG DUMPEN #######

          Dumper.dumpMethodTemplate(modifiedMethod, "buildAndCallMethod/", className = method.classFile.fqn.split("/").last)
          Dumper
            .dumpClassFile(redirectedClass, "classFiles/", "Origin = " + origin)

          // QAMIL:  Werden hier exceptions geworfen?
          val success = callReflectiveMethod(
            modifiedMethod,
            classLoader,
            redirectedClass,
            method,
            sinkInfo,
            associatedMethodString,
            context
          )

          println(if (success) "Erfolg" else "Kein Erfolg")

          if (
            bruteforce && success &&
            modifiedMethod.body.get.instructions.exists(i =>
              i.isInstanceOf[LoadString_W] || i.isInstanceOf[LoadString]
            )
          ) {
            decryptionContextSet += method -> (modifiedMethod, strippedClasses, newClass)
          }
        } else {
          println("Undefined Method template")
        }
      } catch {
        case ex: java.lang.VerifyError =>
          println("Exception " + ex)
          verifyErrors.incrementAndGet()
          if (StringDecryption.logSlicing) {
            StringDecryption.logger.error(parameters.head)
            StringDecryption.logger.error(ex.getMessage)
            StringDecryption.logger.error(ex.getStackTrace.mkString("\n"))
          }
        case ex: java.lang.reflect.InvocationTargetException =>
          println(s"InvocationTargetException $ex, printing Stack Trace")
          ex.printStackTrace()
          invocationTargetError.incrementAndGet()
          if (StringDecryption.logSlicing) {
            StringDecryption.logger.error(parameters.head)
            StringDecryption.logger.error(ex.getMessage)
            StringDecryption.logger.error(ex.getStackTrace.mkString("\n"))
          }
        case ex: java.lang.NullPointerException =>
          println("Exception " + ex)
          ex.printStackTrace()
          nullPointerError.incrementAndGet()
          if (StringDecryption.logSlicing) {
            StringDecryption.logger.error(parameters.head)
            StringDecryption.logger.error(ex.getMessage)
            StringDecryption.logger.error(ex.getStackTrace.mkString("\n"))
          }
        case ex: ParameterUsageException ⇒
          println("Exception " + ex)
          parameterErrors.incrementAndGet()
          if (StringDecryption.logSlicing) {
            StringDecryption.logger.error(parameters.head)
            StringDecryption.logger.error(ex.getMessage)
            StringDecryption.logger.error(ex.getStackTrace.mkString("\n"))
          }

        case ex: java.lang.NoClassDefFoundError =>
          println(
            "Exception " + ex
              .asInstanceOf[java.lang.NoClassDefFoundError]
              .getStackTrace()
              .foreach { stackTraceElement =>
                println(
                  "Stacktrace @ " + stackTraceElement.getFileName + " : " + stackTraceElement.getLineNumber
                )
              }
          )
          noClassDef.incrementAndGet()
          if (StringDecryption.logSlicing) {
            StringDecryption.logger.error(parameters.head)
            StringDecryption.logger.error(ex.getMessage)
            StringDecryption.logger.error(ex.getStackTrace.mkString("\n"))
          }

        case ex: java.util.NoSuchElementException =>
          println("Exception " + ex)
          if (StringDecryption.logSlicing) {
            StringDecryption.logger.error(parameters.head)
            StringDecryption.logger.error(ex.getMessage)
            StringDecryption.logger.error(ex.getStackTrace.mkString("\n"))
          }
        case ex: ThreadDeath =>
          println("Exception " + ex)
          throw ex

        case ex: Throwable =>
          ex.getStackTrace foreach println
          println("Generic Exception LLLL " + ex)
          ex.printStackTrace()
          genericErrors.incrementAndGet()
          if (StringDecryption.logSlicing) {
            StringDecryption.logger.error(parameters.head)
            StringDecryption.logger.error(ex.getMessage)
            StringDecryption.logger.error(ex.getStackTrace.mkString("\n"))
          }

      }
    })
    //runner.setUncaughtExceptionHandler(h)
    runner.start()
    if (debug)
      runner.join()
    else
      runner.join(5000)
    if (runner.isAlive) {
      try {
        classOf[Thread].getMethod("stop").invoke(runner)
      } catch {
        case e: Throwable =>
          StringDecryption.logger.error(parameters.head)
          StringDecryption.logger.error(e.getMessage)
          StringDecryption.logger.error(e.getStackTrace.mkString("\n"))
      }
      timeouts.incrementAndGet()
    }
    if (clean || freeMBytes <= 500) {
      cleanHard()
      clean = false
      System.gc()
    }
  }

  def buildMethod(
      method: Method,
      origin: ValueOrigin,
      context: ClassSlicingContext,
      result: AIResult { val domain: DefaultDomainWithCFGAndDefUse[URL] },
      loi: Int
  )(implicit project: Project[_]): (Option[MethodTemplate], Option[String]) = {

    println(
      "buildMethod: " + method + " with LoI: " + loi + " and origin " + origin + " from class " + method.classFile.fqn
    )

    val slicer = new ClassDeobfuscationSlicer(method, origin, result, loi)
    slicer.doSlice()

    val m = slicer.buildMethodFromSlice(context.dataTypeOfInterest)

    if (
      // Qamil: Sobald einer der ParameterTypen keinem CharSequenceObjectTypes entspricht, wird das MethodTemplate verworfen. WARUM?
      m.descriptor.parameterTypes.exists(
        !typeIsOrHoldsCharSequenceObjectType(_)(project.classHierarchy)
      )
    ) {
      println("buildMethod would return nothing")
      usesParams.incrementAndGet()
      // qamil: TODO: Refactor to only return method
      (Some(m), None)
    } else {
      (Some(m), None)
    }
  }

  // QAMIL: TODO: Nachverfolgen, anhand den dumps schauen, warum die Reflections hier fehlschlagen, oder ob das eine Anti-Deobfuszierungstechnik ist
  def callReflectiveMethod(
                            modifiedMethod: MethodTemplate,
                            classLoader: ClassLoader,
                            cf: ClassFile,
                            method: Method,
                            sinkInfo: SinkInfo,
                            encStringOption: Option[String],
                            context: ClassSlicingContext
  ): Boolean = {

    // qamil: Der ClassLoader ist ein ClassLoader, welcher bereits die Bytedaten der zu ladenen Klassen beinhaltet

    println(
      "call reflective method with modified method " + modifiedMethod.name
    )

    val leakerClass = {
      if (context.dataTypeOfInterest == ObjectType.String)
        classLoader.loadClass("slicing.StringLeaker")
      else classLoader.loadClass("slicing.ByteBufferLeaker")
    }

    // qamil: Hier wird nur der FQN weitergegeben => com.google.android.gms.dynamite.DynamiteModule
    val clazz = classLoader.loadClass(cf.thisType.toJava)
    var successful = false
    //                        try {
    val constructors =
      clazz.getDeclaredConstructors.filter(_.getParameterTypes.length == 0)

    println("loaded classes")

    if (modifiedMethod.name == "<clinit>") {
      println("constructor <clinit>")
      //            attempts.incrementAndGet()
      clazz.getMethod(DUMMYSTATIC).invoke(null)

      successful = logResult(
        leakerClass,
        method,
        sinkInfo,
        encStringOption,
        context
      )
    } else if (modifiedMethod.name == "<init>") {
      println("constructor <init>")
      //            attempts.incrementAndGet()
      constructors(0).setAccessible(true)
      constructors(0).newInstance()

      successful = logResult(
        leakerClass,
        method,
        sinkInfo,
        encStringOption,
        context
      )
    } else if (constructors.nonEmpty || modifiedMethod.isStatic) {
      println("constructor nonEmpty or staticMethod")
      val instance =
        if (modifiedMethod.isStatic) null
        else {
          constructors(0).setAccessible(true)
          constructors(0).newInstance()
        }
      // qamil: Ergibt das nicht immer eine Methode?
      val methods = clazz.getDeclaredMethods
        .filter(m =>
          m.getName == modifiedMethod.name && m.getParameterTypes.sameElements(
            modifiedMethod.parameterTypes.map(_.toJavaClass)
          )
        )
      val jvmMethod = methods(0)

      jvmMethod.setAccessible(true)


      // qamil: Hier werden passende Parametertypen instanziiert, um die Methode aufrufen zu können
      val params: Array[_ <: Object] = jvmMethod.getParameterTypes
        .map{clazz => tryToCreateInstance(clazz, classLoader)}
        .map(
          _.asInstanceOf[Object]
        )

      println(
        "about to invoke a static method or one with a non-empty constructor"
      )

      successful = tryInvoke(
        jvmMethod,
        leakerClass,
        instance.asInstanceOf[Object],
        params,
        method,
        sinkInfo,
        encStringOption,
        context
      )
    } else {
      attempts.decrementAndGet()
      usesParams.incrementAndGet()
      val constructorsWithParams = clazz.getDeclaredConstructors.map(c => {
        c.setAccessible(true)
        c
      })
      attempts.incrementAndGet()
      for (constructor <- constructorsWithParams) {
        try {
          val constructorParams: Array[_ <: Object] =
            constructor.getParameterTypes
              .map{clazz => tryToCreateInstance(clazz, classLoader)}
              .map(
                _.asInstanceOf[Object]
              )
          val instance = constructor.newInstance(constructorParams: _*)

          val jvmMethod = clazz.getDeclaredMethods
            .filter(m =>
              m.getName == modifiedMethod.name && m.getParameterTypes
                .sameElements(modifiedMethod.parameterTypes.map(_.toJavaClass))
            )(0)
          jvmMethod.setAccessible(true)


          val params: Array[_ <: Object] = jvmMethod.getParameterTypes
            .map{clazz => tryToCreateInstance(clazz, classLoader)}
            .map(_.asInstanceOf[Object])


          successful |= tryInvoke(
            jvmMethod,
            leakerClass,
            instance.asInstanceOf[Object],
            params,
            method,
            sinkInfo,
            encStringOption,
            context
          )
        } catch {
          case _: Throwable => println("Unhandled Exception")
        }
      }
    }
    successful
  }

  def tryToCreateInstance[T](clazz: Class[_], classLoader: ClassLoader): Any = {
    if (clazz.isPrimitive) {
      //throw new NullPointerException()
      ClassUtils.primitiveToWrapper(clazz) match {
        case w if w == classOf[Integer]           => Integer.valueOf(0)
        case w if w == classOf[java.lang.Boolean] => java.lang.Boolean.FALSE
        case w if w == classOf[java.lang.Long]    => java.lang.Long.valueOf(0L)
        case w if w == classOf[java.lang.Short] =>
          java.lang.Short.valueOf(0.asInstanceOf[Short])
        case w if w == classOf[java.lang.Byte] =>
          java.lang.Byte.valueOf(0.asInstanceOf[Byte])
        case w if w == classOf[java.lang.Character] =>
          java.lang.Character.valueOf('a')
        case w if w == classOf[java.lang.Float] => java.lang.Float.valueOf(0.0f)
        case w if w == classOf[java.lang.Double] =>
          java.lang.Double.valueOf(0.0)
      }
    } else {

      val const = clazz.getDeclaredConstructors.find(c =>
        c.getParameterTypes.isEmpty && c.isAccessible
      )
      if (const.isDefined) {
        const.get.newInstance()
      } else {
        tryToMockInstance(clazz, classLoader)
      }
    }
  }

  def tryToMockInstance(classToMock: Class[_], classLoader: ClassLoader) : Any = {
    val className = classToMock.getName

    if (className == "android.content.Context") {
      return new AndroidLibraryMocker(apkManager).mockContext(classLoader)
    }

    null
  }

  def fixTime(cf: ClassFile): ClassFile = {
    val filteredMethods = cf.methods.map[MethodTemplate] { m =>
      var skip = 0
      val body = m.body.map(body =>
        Code(
          body.maxStack,
          body.maxLocals,
          body.instructions.map {
            case MethodInvocationInstruction(
                  ObjectType.System,
                  _,
                  "currentTimeMillis",
                  _
                ) =>
              skip = 2
              LCONST_0
            case null if skip > 0 => skip -= 1; NOP
            case ins              => ins
          },
          body.exceptionHandlers,
          body.attributes
        )
      )
      m.copy(body = body)
    }

    cf.copy(methods = filteredMethods)
  }

  def redirectToDummyClass(
      cf: ClassFile,
      dummyClass: ClassFile,
      superType: Option[ObjectType],
      isInitChange: Boolean
  ): (ClassFile, ClassFile) = {
    var newClass = cf
    var newDummy = dummyClass
    if (isInitChange)
      cf.methods.toSet
        .map(m =>
          makeRef2DummyInit(
            m,
            newClass,
            if (superType.isDefined) superType.get else ObjectType.Object,
            newDummy
          )
        )
        .foreach { res: (MethodTemplate, Set[MethodTemplate]) =>
          newClass = addMethodToClass(newClass, res._1)
          for (m <- res._2)
            newDummy = addMethodToClass(newDummy, m)
        }
    (newClass, newDummy)
    //cf
  }

  def makeRef2DummyInit(
      method: Method,
      cf: ClassFile,
      superType: ObjectType,
      dummyClass: ClassFile
  ): (MethodTemplate, Set[MethodTemplate]) = {
    var methodTmpSet = Set.empty[MethodTemplate]
    val code = method.body.map { body ⇒
      val instructions = body.instructions.map {
        case inst: INVOKESPECIAL if inst.declaringClass.fqn == superType.fqn =>
          val tmp = buildDummySpecialConsumerMethod(inst, dummyClass)
          methodTmpSet += tmp
          INVOKESPECIAL(
            dummyClass.thisType,
            isInterface = false,
            "<init>",
            tmp.descriptor
          )
        case inst: MethodInvocationInstruction
            if inst.declaringClass.isObjectType && inst.declaringClass.asObjectType.fqn == superType.fqn =>
          val (ins, methodTmp) =
            getDefaultValueForType(inst, cf, dummyClass, methodTmpSet)
          methodTmpSet += methodTmp
          ins
        case a => a
      }
      body.copy(instructions = instructions)
    }
    (method.copy(body = code), methodTmpSet)
  }

  def generateFreeName(setNames: Set[String]): String = {
    var count = 0
    while (setNames.contains("dummyConsumer" + count)) count += 1
    "dummyConsumer" + count
  }

  def buildNewRefArray[T <: AnyRef](
      first: List[T],
      second: List[T]
  ): RefArray[T] = {
    RefArray._UNSAFE_from((first ++ second).toArray)
  }

  def buildDummySpecialConsumerMethod(
      t: INVOKESPECIAL,
      cf: ClassFile
  ): MethodTemplate = {
    import org.opalj.ba._
    METHOD(
      PUBLIC,
      "<init>",
      MethodDescriptor
        .apply(t.methodDescriptor.parameterTypes, VoidType)
        .toJVMDescriptor,
      CODE(
        ALOAD_0,
        INVOKESPECIAL(
          ObjectType.Object,
          isInterface = false,
          "<init>",
          MethodDescriptor.NoArgsAndReturnVoid
        ),
        RETURN
      )
    ).result(cf.version, cf.thisType)._1
  }

  def removeAndroidSystemCalls(
      method: Method,
      cf: ClassFile,
      dummyClass: ClassFile
  ): Set[MethodTemplate] = {
    var set = Set.empty[MethodTemplate]
    val code = method.body.map { body ⇒
      val instructions = body.instructions.map {
        case NEW(ot)
            if project.isLibraryType(ot) && (ot.fqn.startsWith(
              "android"
            ) || ot.fqn.startsWith("dalvik")) ⇒
          ACONST_NULL
        case ins @ MethodInvocationInstruction(rt @ ObjectType(_), _, _, _) =>
          val res =
            if (
              project.isLibraryType(rt) && (rt.fqn.startsWith(
                "android"
              ) || rt.fqn.startsWith("dalvik"))
            ) {
              val (inst, methodTmp) =
                getDefaultValueForType(ins, cf, dummyClass, set)
              set += methodTmp
              inst
            } else ins
          res
        case ins ⇒ ins
      }
      body.copy(instructions = instructions)
    }
    set += method.copy(body = code)
    set
  }

  def getDefaultValueForType(
      t: MethodInvocationInstruction,
      cf: ClassFile,
      dummyClass: ClassFile,
      methodTmpSet: Set[MethodTemplate]
  ): (Instruction, MethodTemplate) = {
    // TODO: Maybe check for wrapper classes
    val freeName = generateFreeName(
      (cf.methods.toSet.map((m: Method) => m.name) ++ methodTmpSet.map(m =>
        m.name
      ) ++ dummyClass.methods.toSet.map((m: Method) => m.name)).toSet[String]
    )
    val methodTmp = buildDummyConsumeMethod(
      freeName,
      t,
      cf,
      getDefaultInstructionsForType(t.methodDescriptor.returnType)
    )
    (
      INVOKESTATIC(
        DummyClass.objectType,
        isInterface = false,
        freeName,
        methodTmp.descriptor
      ),
      methodTmp
    )
  }

  def getDefaultInstructionsForType(
      t: Type
  ): (CodeElement[_], CodeElement[_]) = {
    t match {
      case a if a.isIntLikeType ⇒ (ICONST_0, IRETURN)
      case _: VoidType ⇒
        (NOP, RETURN)
      case _: LongType ⇒
        (LCONST_0, LRETURN)
      case _: DoubleType ⇒
        (DCONST_0, DRETURN)
      case _: FloatType ⇒
        (FCONST_0, FRETURN)
      case _: BooleanType ⇒
        (ICONST_0, IRETURN)
      case a if a.isObjectType ⇒
        (ACONST_NULL, ARETURN)
      case a if a.isReferenceType ⇒
        (ACONST_NULL, ARETURN)
    }
  }

  def buildDummyConsumeMethod(
      name: String,
      t: MethodInvocationInstruction,
      cf: ClassFile,
      instTpl: (CodeElement[_], CodeElement[_])
  ): MethodTemplate = {
    import org.opalj.ba._
    if (t.isInstanceMethod || t.isInterfaceCall || t.isVirtualMethodCall) {
      val newRefArray = RefArray._UNSAFE_from(
        (List(
          t.declaringClass
        ) ++ t.methodDescriptor.parameterTypes.toList).toArray
      )
      METHOD(
        PUBLIC.STATIC,
        name,
        MethodDescriptor
          .apply(newRefArray, t.methodDescriptor.returnType)
          .toJVMDescriptor,
        CODE(
          instTpl._1,
          instTpl._2
        )
      ).result(cf.version, cf.thisType)._1
    } else
      METHOD(
        PUBLIC.STATIC,
        name,
        MethodDescriptor
          .apply(
            t.methodDescriptor.parameterTypes,
            t.methodDescriptor.returnType
          )
          .toJVMDescriptor,
        CODE(
          instTpl._1,
          instTpl._2
        )
      ).result(cf.version, cf.thisType)._1
  }



  def filterMethodsAndFields(
      cf: ClassFile,
      relevantMethods: Set[Method],
      relevantFields: Set[Field]
  ): ClassFile = {
    //    cf.copy(
    //            fields = modifiedClass.fields.filter(relevantFields.contains(_)).map(_.copy()),
    //      methods = cf.methods.filter(relevantMethods.contains(_)).map(_.copy()))
    cf.copy(accessFlags = cf.accessFlags | ACC_PUBLIC.mask)
  }

  def buildDummyStatic(cf: ClassFile): MethodTemplate = {
    import org.opalj.ba._
    METHOD(
      PUBLIC.STATIC,
      DUMMYSTATIC,
      MethodDescriptor.NoArgsAndReturnVoid.toJVMDescriptor,
      CODE(
        RETURN
      )
    ).result(cf.version, cf.thisType)._1
  }

  val DUMMYSTATIC = "DUMMYSTATIC"

  def addMethodToClass(cf: ClassFile, mt: MethodTemplate): ClassFile = {
    var copiedMethods = cf.methods
      .filterNot(m ⇒ m.descriptor == mt.descriptor && m.name == mt.name)
      .map[MethodTemplate](_.copy())
    if (mt.name == "<clinit>") {
      copiedMethods :+= buildDummyStatic(cf)
    }
    cf.copy(methods = mt +: copiedMethods, version = bi.Java5Version)
  }

  def handleAllCallsToNewMethod(
      cf: ClassFile,
      newMethod: MethodTemplate
  ): ClassFile = {
    var allMethods = cf.methods.toSet.map((m: Method) => m.copy())
    cf.methods.foreach { m =>
      if (m.body.isDefined) {
        val code = m.body.map { body =>
          val newInstructions = body.instructions.map {
            case mi: MethodInvocationInstruction
                if mi.name == newMethod.name && mi.methodDescriptor == newMethod.descriptor =>
              val freeName =
                generateFreeName(cf.methods.toSet.map((m: Method) => m.name))
              val methodTmp = buildDummyConsumeMethod(
                freeName,
                mi,
                cf,
                getDefaultInstructionsForType(mi.methodDescriptor.returnType)
              )
              allMethods += methodTmp
              INVOKESTATIC(
                cf.thisType,
                isInterface = false,
                freeName,
                methodTmp.descriptor
              )
            case i => i
          }
          body.copy(instructions = newInstructions)
        }
        allMethods = allMethods.filterNot(m1 =>
          m1.name == m.name && m1.descriptor == m.descriptor
        ) + m.copy(body = code)
      }
    }
    cf.copy(methods = buildNewRefArray(allMethods.toList, List()))
  }

  val stackMapCache =
    new java.util.concurrent.ConcurrentHashMap[Method, StackMapTable]()

  def addStackMapTable(m: Method, ch: ClassHierarchy): MethodTemplate = {
    val stackMapTable = stackMapCache.computeIfAbsent(
      m,
      m ⇒ CodeAttributeBuilder.computeStackMapTable(m)(ch)
    )
    m.copy(attributes = m.attributes :+ stackMapTable)
  }

  val NON_ABSTRACT = 0xfbff

  def handleAbstractMethods(modifiedClass: ClassFile): List[MethodTemplate] = {
    modifiedClass.methods.filter(m => m.isAbstract).toList.map { m =>
      val defaultInstructions = getDefaultInstructionsForType(m.returnType)
      METHOD(
        PUBLIC,
        m.name,
        MethodDescriptor.apply(m.parameterTypes, m.returnType).toJVMDescriptor,
        CODE(
          defaultInstructions._1,
          defaultInstructions._2
        )
      ).result(modifiedClass.version, modifiedClass.thisType)._1
    }
  }

  def handleMissingMethods(
      filteredClass: (ClassFile, ClassFile),
      originalClass: ClassFile,
      project: Project[_]
  ): (ClassFile, ClassFile) = {
    var allMethods: Set[MethodTemplate] =
      filteredClass._1.methods.toSet.map((m: Method) => m.copy())
    var newDummyMethods =
      filteredClass._2.methods.toSet.map((m: Method) => m.copy())
    filteredClass._1.methods.foreach { method =>
      if (method.body.isDefined) {
        method.body.get.instructions.foreach {
          case mi: MethodInvocationInstruction
              if mi.declaringClass == filteredClass._1.thisType =>
            if (mi.declaringClass == filteredClass._1.thisType) {
              var found = filteredClass._1.methods.find(m1 =>
                m1.name == mi.name && mi.methodDescriptor == m1.descriptor
              )
              if (found.isEmpty) {
                found = originalClass.methods.find(m1 =>
                  m1.name == mi.name && mi.methodDescriptor == m1.descriptor
                )
                if (
                  found.isEmpty && originalClass.superclassType.isDefined && project
                    .classFile(originalClass.superclassType.get)
                    .isDefined
                ) {
                  found = project
                    .classFile(originalClass.superclassType.get)
                    .get
                    .methods
                    .find(m1 =>
                      m1.name == mi.name && mi.methodDescriptor == m1.descriptor
                    )
                  if (found.isEmpty) {
                    if (mi.isVirtualMethodCall) {
                      newDummyMethods += buildDummyMethod(
                        mi.name,
                        mi,
                        filteredClass._2
                      )
                    } else {
                      allMethods += buildDummyMethod(
                        mi.name,
                        mi,
                        filteredClass._1
                      )
                    }
                  } else {
                    if (found.get.body.isDefined) {
                      if (mi.isVirtualMethodCall) {
                        newDummyMethods += buildDummyMethod(
                          mi.name,
                          mi,
                          filteredClass._2
                        )
                      } else {
                        allMethods += buildDummyMethod(
                          mi.name,
                          mi,
                          filteredClass._1
                        )
                      }
                    } else {
                      if (mi.isVirtualMethodCall) {
                        newDummyMethods += buildDummyMethod(
                          mi.name,
                          mi,
                          filteredClass._2
                        )
                      } else {
                        allMethods += buildDummyMethod(
                          mi.name,
                          mi,
                          filteredClass._1
                        )
                      }
                    }
                  }
                } else {
                  if (mi.isVirtualMethodCall) {
                    newDummyMethods += buildDummyMethod(
                      mi.name,
                      mi,
                      filteredClass._2
                    )
                  } else {
                    allMethods += buildDummyMethod(
                      mi.name,
                      mi,
                      filteredClass._1
                    )
                  }
                }
              }
            }
          case _ =>
        }
      }
    }
    (
      filteredClass._1.copy(methods =
        buildNewRefArray(allMethods.toList, List[Method]())
          .map[MethodTemplate](_.copy())
      ),
      filteredClass._2.copy(methods =
        buildNewRefArray(newDummyMethods.toList, List[Method]())
          .map[MethodTemplate](_.copy())
      )
    )
  }

  def buildDummyMethod(
      name: String,
      t: MethodInvocationInstruction,
      cf: ClassFile
  ): MethodTemplate = {
    val instTpl = getDefaultInstructionsForType(t.methodDescriptor.returnType)
    import org.opalj.ba._
    if (t.isInstanceMethod) {
      METHOD(
        PUBLIC,
        name,
        MethodDescriptor
          .apply(
            t.methodDescriptor.parameterTypes,
            t.methodDescriptor.returnType
          )
          .toJVMDescriptor,
        CODE(
          instTpl._1,
          instTpl._2
        )
      ).result(cf.version, cf.thisType)._1
    } else
      METHOD(
        PUBLIC.STATIC,
        name,
        MethodDescriptor
          .apply(
            t.methodDescriptor.parameterTypes,
            t.methodDescriptor.returnType
          )
          .toJVMDescriptor,
        CODE(
          instTpl._1,
          instTpl._2
        )
      ).result(cf.version, cf.thisType)._1
  }

  def freeMBytes(): Long = {
    Runtime.getRuntime.freeMemory / 1024 / 1024
  }



  def cleanHard(): Unit = {
    Thread.getAllStackTraces.keySet().toArray().foreach { f =>
      val th = f.asInstanceOf[Thread]
      if (
        !allThreads.contains(th.getId) &&
        !th.getName.startsWith("org.opalj") &&
        !th.getName.startsWith("scala-execution-context-") &&
        !th.getName.contains("RMI") &&
        !th.getName.contains("JMX") &&
        !th.getName.contains("GC") &&
        !th.getName.contains("Attach Listener") &&
        !th.getName.contains("Signal Dispatcher") &&
        !th.getName.contains("Finalizer") &&
        !th.getName.contains("Reference Handler") &&
        !th.getName.equals("main") &&
        !th.getName.contains("Monitor")
      ) {
        try {
          //println(th.getName + " -> " + th.getId)
          th.interrupt()
          th.interrupt()
        } catch {
          case _: Throwable => println("Unhandled THrowable")
        }
        try {
          val runnableField = th.getClass.getDeclaredField("target")
          runnableField.setAccessible(true)
          val r = runnableField.get(th).asInstanceOf[Runnable]
          transitiveKill(r)
        } catch {
          case _: Throwable => println("Unhandled Throwable")
        }
        th.stop()
      }

    }
  }

  def transitiveKill(r: Object): Unit = {
    for (field <- r.getClass.getFields) {
      field.setAccessible(true)
      field.getType.getTypeName match {
        case "boolean" | "Z" => field.setBoolean(r, false)
        case "byte" | "B"    => field.setByte(r, 0)
        case "char" | "C"    => field.setChar(r, 0)
        case "double" | "D"  => field.setDouble(r, 0.0)
        case "float" | "F"   => field.setFloat(r, 0.0f)
        case "int" | "I"     => field.setInt(r, 0)
        case "long" | "J"    => field.setLong(r, 0)
        case "short" | "S"   => field.setShort(r, 0)
        case _ =>
          val o = field.get(r)
          transitiveKill(o)
          field.set(r, null)
      }
    }
  }

  def findInvokedInterfaceMethodsInSlice(
      modifiedMethod: MethodTemplate,
      interfaceTypes: Seq[ObjectType]
  ): RefArray[ObjectType] = {
    val calledMethods = modifiedMethod.body.get.collect {
      case ins: INVOKEINTERFACE ⇒ ins.declaringClass
    }
    RefArray.from(
      interfaceTypes
        .intersect(calledMethods)
        .toArray
        .asInstanceOf[Array[AnyRef]]
    )
  }

  def tryInvoke(
      jvmMethod: java.lang.reflect.Method,
      // qamil: Die resultClass könnte der StringLeaker sein
      resultClass: Class[_],
      instance: Object,
      params: Array[_ <: Object],
      originalMethod: Method,
      sinkInfo: SinkInfo,
      encStringOption: Option[String],
      context: ClassSlicingContext
  ): Boolean = {
    println("invoking reflection")
    // TODO:
    try {
      jvmMethod.invoke(instance, params: _*)
      println("invoked reflection")
      logResult(resultClass, originalMethod, sinkInfo, encStringOption, context)
    } catch {
      case e : Throwable => println(s"tryInvoke Exception $e");  false
    }
  }

  def logResult(
      resultClass: Class[_],
      originalMethod: Method,
      sinkInfo: SinkInfo,
      encStringOption: Option[String],
      context: ClassSlicingContext
  ): Boolean = {
    println("logResult: Getting Result field")
    val resultField = resultClass.getDeclaredField("result")
    println("logResult: Setting Result Field Accessible")
    resultField.setAccessible(true)
    if (context.dataTypeOfInterest == ObjectType.String) {
      println("logResult: The Type of Interest is a String")
      val res = resultField.get(null).asInstanceOf[String]
      if (res.nonEmpty && res != "null") {
        // qamil: TODO: Check whether that can stay as is and how it should be handled
         val cl = false // StringClassifier.classify(res)
        if (
          (sinkInfo.sinkMethod != "bruteForce" || !cl) && (encStringOption.isEmpty ||
          !res.contains(encStringOption.get))
        ) {
          var encString: String = "-"
          if (encStringOption.isDefined)
            encString = encStringOption.get.replaceAll("[\n\r;]", "")
          val escapedString = res.replaceAll("[\n\r;]", "")
          var ratio = 1.0
          if (res.length > 0) {
            ratio = removeConstantStrings(res).length.toDouble / res.length
          }
          resultStream.append(
            s"${originalMethod.classFile.fqn};${originalMethod.signature.toJava};$ratio;${if (cl) 1
            else 0};$encString;$escapedString\n"
          )
          resultStream.flush()
          //unfilteredResults += ((s"${originalMethod.classFile.fqn}:${originalMethod.signature.toJava}", res))
          successful.incrementAndGet()
        }
        return true
      }
      false
    } else {
      apkManager.leaker.leakResult(resultField, context)
      true
    }

  }

  def removeConstantStrings(result: String): String = {
    var str = result
    if (constantStrings.contains(str + "_adfeiwo")) {
      ""
    } else {
      for (string <- stringUsages) {
        str = str.replace(string, "")
        if (str.isEmpty) return str
      }
      str
    }
  }

  /// qamil: Gibt an, ob es sich bei dem zu checkenden Typen um einen CharSequenceObjectType
  /// oder einen Array mit Elementen eines solchen Typens handelt
  def typeIsOrHoldsCharSequenceObjectType(
      typeToCheck: Type
  )(implicit classHierarchy: ClassHierarchy): Boolean = {
    var objectType: Option[ObjectType] = None

    if (typeToCheck.isObjectType) {
      objectType = Some(typeToCheck.asObjectType)
    } else if (typeToCheck.isArrayType) {
      val elementType = typeToCheck.asArrayType.elementType
      if (elementType.isObjectType) {
        objectType = Some(elementType.asObjectType)
      }
    }

    objectType.exists { ot ⇒
      classHierarchy.isSubtypeOf(ot, CharSequenceObjectType) || // OrUnkown?!
      classHierarchy
        .allSubtypes(ot, reflexive = true)
        .contains(CharSequenceObjectType)
    }
  }

  def getAccessedFieldsMethodsAndClasses(
      cf: ClassFile,
      relevantMethod: Option[Method],
      visitedClasses: scala.collection.mutable.Set[
        (ClassFile, Option[Method])
      ] = scala.collection.mutable.Set.empty[(ClassFile, Option[Method])]
  )(implicit
      modifiedClassFile: ClassFile,
      project: Project[_]
  ): (Set[Method], Set[Field], Set[ClassFile]) = {
    // qamil: Zunächst sind all die Methoden relevant, die initializer sind, UND
    // die relevante Methoden
    var relevantMethods = cf.methods
      .filter(m =>
        m.name == "<clinit>" ||
          (m.name == "<init>" /*&& relevantMethod.exists(!_.isStatic)*/ )
          || relevantMethod.exists(relM =>
            relM.name == m.name && relM.descriptor == m.descriptor
          )
      )
      .toSet


    var toVisit = relevantMethods
    var visitedMethods = toVisit
    visitedClasses.add((cf, relevantMethod))
    var relevantFields = Set[Field]()

    // qamil: Die relevanten Klassen sind zunächst die Klasse mitsamt all ihren Interfaces und Superklassen
    // , die im Projekt vorhanden sind UND die modifizierte Klasse
    var relevantClasses =
      Set(cf) ++ cf.interfaceTypes.flatMap[ClassFile](project.classFile(_)) // ++
        // TODO: REIN EXPERIMENTELL, ZUM TESTEN OB DER BUG HIER LIEGT
       // project.classFile(ObjectType("com/google/android/gms/common/GooglePlayServicesNotAvailableException"))
    // ++ project.classFile(ObjectType("com/google/android/gms/common/GooglePlayServicesRepairableException"))
    cf.superclassType.foreach(
      project.classFile(_).foreach(relevantClasses += _)
    )

    // TODO: Die Exception ist hier logischerweise noch nicht Teil der relevant Classes
    //relevantClasses.filter(cf => cf.fqn.contains("GooglePlayServicesNotAvailableException")) foreach {cf => println("ClassFile Contained " + cf.fqn)}

    relevantClasses = relevantClasses.filter(cf =>
      (cf eq modifiedClassFile) || project.allProjectClassFiles.contains(cf)
    )

    // TODO: Warum wird hier gemappt? Ist das nicht einfach eine einzelne Option?
    def lookupClassFile(
        cfOption: Option[ClassFile],
        methodOption: Option[Method] = None
    ): Unit = {
      cfOption
        .map(cf =>
          if (cf.fqn == modifiedClassFile.fqn) modifiedClassFile else cf
        )
        .foreach(cf =>
          if (
            !libraryClasses.contains(cf)
            && (!visitedClasses.contains((cf, methodOption))
            || methodOption.exists(!relevantMethods.contains(_)))
          ) {
            val (addedMethods, addedFields, addedClasses) =
              getAccessedFieldsMethodsAndClasses(
                cf,
                methodOption,
                visitedClasses
              )
            relevantMethods ++= addedMethods
            relevantFields ++= addedFields
            relevantClasses ++= addedClasses
          }
        )
    }

    while (toVisit.nonEmpty) {
      val methodToVisit = toVisit.head
      visitedMethods += methodToVisit
      toVisit = toVisit.tail
      if (methodToVisit.exceptionTable.isDefined) {
        methodToVisit.exceptionTable.get.exceptions.foreach(ex =>
          lookupClassFile(project.classFile(ex))
        )
      }
      if (methodToVisit.body.isDefined) {
        methodToVisit.body.get.exceptionHandlers.foreach(ex =>
          if (ex.catchType.isDefined)
            lookupClassFile(project.classFile(ex.catchType.get))
        )
      }
      methodToVisit.descriptor.parameterTypes
        .foreach(parameterType =>
          if (parameterType.isObjectType)
            lookupClassFile(project.classFile(parameterType.asObjectType))
          else if (parameterType.isArrayType && parameterType.asArrayType.elementType.isObjectType)
            lookupClassFile(
              project.classFile(parameterType.asArrayType.elementType.asObjectType)
            )
        )
      if (methodToVisit.descriptor.returnType.isObjectType) {
        lookupClassFile(project.classFile(methodToVisit.descriptor.returnType.asObjectType))
      } else if (
        methodToVisit.descriptor.returnType.isArrayType && methodToVisit.descriptor.returnType.asArrayType.elementType.isObjectType
      )
        lookupClassFile(
          project.classFile(
            methodToVisit.descriptor.returnType.asArrayType.elementType.asObjectType
          )
        )
      methodToVisit.body.foreach(_.instructions.foreach {
        case FieldAccess(base, name, ty) =>
          if (ty.isObjectType)
            lookupClassFile(project.classFile(ty.asObjectType))
          project
            .classFile(base)
            .foreach(cf => {
              cf.findField(name, ty).foreach(relevantFields += _)
              lookupClassFile(Some(cf))
            })
        case MethodInvocationInstruction(base, isInterface, name, desc)
            if base.isObjectType && !isInterface =>
          //println("MethodInvication: " + base + " " +  isInterface + " " + name + " " + desc)
          project
            .classFile(base.asObjectType)
            .foreach(targetCf => {
              val callee =
                project.resolveMethodReference(base.asObjectType, name, desc)

              callee.foreach(relevantMethods += _)

              if (cf == targetCf) {
                callee.foreach(m =>
                  if (!visitedMethods(m)) {
                    toVisit += m
                  }
                )
              } else {
                lookupClassFile(Some(targetCf), callee)
              }
            })
        case NEW(ty) =>
          //println("NEW: " + ty.fqn)
          lookupClassFile(project.classFile(ty))
        case ANEWARRAY(ty) if ty.isObjectType =>
          lookupClassFile(project.classFile(ty.asObjectType))
        case MULTIANEWARRAY(ty, _) if ty.isObjectType =>
          lookupClassFile(project.classFile(ty.asObjectType))
        case INSTANCEOF(ty) if ty.isObjectType =>
          lookupClassFile(project.classFile(ty.asObjectType))
        case LDCClass(ty) if ty.isObjectType =>
          lookupClassFile(project.classFile(ty.asObjectType))
        case e => //println(s"Another case in the method has occured: $e")
      })
    }

    //Thread.sleep(10000)

    val exceptions = project.allClassFiles filter {classFile => project.classHierarchy.allSupertypes(classFile.thisType, false).contains(ObjectType.Exception)}

    relevantClasses ++= exceptions

    (relevantMethods, relevantFields, relevantClasses)
    //(relevantMethods, relevantFields, project.allClassFiles.toSet)
  }


  def teardownAnalysis(
      t0: Long,
      t1: Long,
      t2: Long,
      t3: Long,
      start: Instant
  ): Unit = {
    val t4 = System.currentTimeMillis()
    System.setErr(err)
    System.setOut(out)
    println(
      "Write results to -> " + StringDecryption.outputDir + "/results/" + parameters.head + ".txt"
    )

    val end = Instant.now()
    val time = ChronoUnit.MILLIS.between(start, end)

    val allInstructionCounts = project.allMethodsWithBody
      .map(m => m.body.get.instructionsCount)
      .toList
      .sorted
    logStream.write(
      Array(
        parameters.head,
        t1 - t0,
        t2 - t1,
        t3 - t2,
        t4 - t3,
        t4 - t0,
        project.allProjectClassFiles.size,
        project.allMethods.size,
        allInstructionCounts.sum.toDouble / (if (allInstructionCounts.nonEmpty)
                                               allInstructionCounts.size
                                             else 1),
        if (allInstructionCounts.nonEmpty)
          allInstructionCounts(allInstructionCounts.size / 2)
        else 0,
        if (allInstructionCounts.nonEmpty) allInstructionCounts.max else 0,
        allInstructionCounts.sum,
        constantStrings.size,
        successful,
        attempts
      ).mkString("", ";", "\n")
    )
    logStream.close()

    resultStream.close()
  }

  class SinkInfo(
      val sinkDeclaringClass: ReferenceType,
      val sinkMethod: String,
      val sinkPC: Int
  )

}

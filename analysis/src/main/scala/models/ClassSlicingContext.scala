package models

import org.opalj.br.{FieldType, ObjectType}
import org.opalj.br.instructions.MethodInvocationInstruction

object ClassSlicingContext {

  def fromConstructorInvocation(invocation: MethodInvocationInstruction, parameterType: FieldType) : Option[ClassSlicingContext] = {
    val constructorsDeclaringClass = invocation.declaringClass
    if (parameterType.isObjectType) {
      //println("Creating ClassSlicingContext with ")
      Some(new ClassSlicingContext(parameterType.asObjectType, invocation.declaringClass.asObjectType))
    } else {
      None
    }
  }
}

case class ClassSlicingContext(
    dateTypeOfInterest: ObjectType,
    dataDestination: ObjectType,
                              )

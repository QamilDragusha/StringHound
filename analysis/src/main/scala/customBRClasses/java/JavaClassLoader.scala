package customBRClasses.java

import customBRClasses.CustomBRClass
import org.opalj.ba.{CLASS, CODE, FIELDS, METHOD, METHODS, PRIVATE, PUBLIC, STATIC}
import org.opalj.bi
import org.opalj.br.instructions.{ALOAD_0, INVOKESPECIAL, RETURN}
import org.opalj.br.{ClassFile, MethodDescriptor, ObjectType}

object JavaClassLoader extends CustomBRClass {

  private def thisType = "java/lang/ClassLoader"

  override def objectType: ObjectType = ObjectType(thisType)

  override def classFile: ClassFile = {
    CLASS(
      version = bi.Java5Version,
      accessModifiers = PUBLIC,
      thisType = thisType,
      fields = FIELDS(),
      methods = METHODS(
        METHOD(
          STATIC.PUBLIC,
          "<init>",
          MethodDescriptor.NoArgsAndReturnVoid.toJVMDescriptor,
          CODE(
            ALOAD_0,
            INVOKESPECIAL(ObjectType.Object, isInterface = false, "<init>", MethodDescriptor.NoArgsAndReturnVoid),
            RETURN,
          )
        )
      ),
    ).toBR._1
  }

}

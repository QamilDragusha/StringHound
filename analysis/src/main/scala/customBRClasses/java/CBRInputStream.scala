package customBRClasses.java

import customBRClasses.CustomBRClass
import org.opalj.ba.{CLASS, CODE, FIELDS, METHOD, METHODS, PUBLIC, STATIC}
import org.opalj.bi
import org.opalj.br.instructions.{
  ALOAD_0,
  BASTORE,
  BIPUSH,
  INVOKESPECIAL,
  IRETURN,
  L2I,
  POP,
  RETURN,
  SIPUSH
}
import org.opalj.br.{ArrayType, ClassFile, MethodDescriptor, ObjectType}

object CBRInputStream extends CustomBRClass {

  private def thisType = "java/io/InputStream"

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
            INVOKESPECIAL(
              ObjectType.Object,
              isInterface = false,
              "<init>",
              MethodDescriptor.NoArgsAndReturnVoid
            ),
            RETURN
          )
        ),
        METHOD(
          PUBLIC,
          "read",
          MethodDescriptor(
            ArrayType(1, ObjectType.Byte),
            ObjectType.Integer
          ).toJVMDescriptor,
          CODE(
            // Ich bekomme einen ArrayRef, der liegt auf dem Stack
            // SIPUSH(0), // index
            // BIPUSH(1), // value
            // BASTORE, // arrayref, index, value -> ...
            POP,
            SIPUSH(0),
            L2I, // JUST LAZY; change to directly push int if possible
            IRETURN
          )
        ),
        METHOD(
          PUBLIC,
          "close",
          MethodDescriptor.NoArgsAndReturnVoid.toJVMDescriptor,
          CODE(
            RETURN
          )
        )
      )
    ).toBR._1
  }

}
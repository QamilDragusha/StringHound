package customBRClasses.java

import customBRClasses.CustomBRClass
import org.opalj.ba.{CLASS, CODE, FIELDS, METHOD, METHODS, PUBLIC, STATIC}
import org.opalj.bi
import org.opalj.br.instructions.{ALOAD_0, ARETURN, ASTORE_0, DUP, INVOKESPECIAL, NEW, RETURN}
import org.opalj.br.{ArrayType, ClassFile, MethodDescriptor, ObjectType}

object CBRByteBuffer extends CustomBRClass {

  private def thisType = "java/nio/ByteBuffer"

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
        ),
        METHOD(
          STATIC.PUBLIC,
          "wrap",
          MethodDescriptor(ArrayType(1, ObjectType.Byte), this.objectType).toJVMDescriptor,
          CODE(
            NEW(this.objectType),
            DUP,
            INVOKESPECIAL(this.objectType, isInterface = false, "<init>", methodDescriptor = MethodDescriptor.NoArgsAndReturnVoid),
            ARETURN,
          ),
        ),
      ),
    ).toBR._1
  }

}

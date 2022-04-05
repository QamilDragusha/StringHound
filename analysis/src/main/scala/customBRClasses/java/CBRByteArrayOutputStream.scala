package customBRClasses.java

import customBRClasses.CustomBRClass
import org.opalj.ba.{CLASS, CODE, FIELDS, METHOD, METHODS, PUBLIC, STATIC}
import org.opalj.{bi, br}
import org.opalj.br.instructions.{ALOAD_0, ARETURN, INVOKESPECIAL, NEWARRAY, POP, RETURN}
import org.opalj.br.{ArrayType, ClassFile, FieldTypes, MethodDescriptor, NoFieldTypes, ObjectType, VoidType}

object CBRByteArrayOutputStream extends CustomBRClass {

  private def thisType = "java/io/ByteArrayOutputStream"

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
          PUBLIC,
          "write",
          MethodDescriptor(FieldTypes(ArrayType(1, ObjectType.Byte), ObjectType.Integer, ObjectType.Integer),VoidType).toJVMDescriptor,
          CODE(
            POP,
            POP,
            POP,
            RETURN,
          ),
        ),
        METHOD(
          PUBLIC,
          "flush",
          MethodDescriptor.NoArgsAndReturnVoid.toJVMDescriptor,
          CODE(RETURN),
        ),
        METHOD(
          PUBLIC,
          "close",
          MethodDescriptor.NoArgsAndReturnVoid.toJVMDescriptor,
          CODE(RETURN),
        ),
        METHOD(
          PUBLIC,
          "toByteArray",
          MethodDescriptor.withNoArgs(ArrayType(1, ObjectType.Byte)).toJVMDescriptor,
          CODE(
            NEWARRAY(8), // Array Type Code 8 => Byte
            ARETURN,
          ),
        ),
      ),
    ).toBR._1
  }

}

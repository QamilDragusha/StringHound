package customBRClasses.leakers
import org.opalj.ba.{CLASS, CODE, FIELD, FIELDS, METHOD, METHODS, PUBLIC, STATIC}
import org.opalj.bi
import org.opalj.br.{ArrayType, ClassFile, IntegerType, MethodDescriptor, ObjectType, VoidType}
import org.opalj.br.instructions.{AASTORE, ALOAD, ALOAD_0, ALOAD_3, ANEWARRAY, ARRAYLENGTH, ASTORE, ASTORE_3, DUP, GETSTATIC, IADD, ICONST_0, ICONST_1, INVOKESTATIC, INVOKEVIRTUAL, ISTORE_1, PUTSTATIC, RETURN}
import org.opalj.collection.immutable.RefArray

object StringLeaker extends Leaker {

  private def thisType = "slicing/StringLeaker"

  override def objectType: ObjectType = ObjectType(thisType)

  override def classFile: ClassFile = {
    CLASS(
      version = bi.Java5Version,
      accessModifiers = PUBLIC,
      thisType = thisType,
      fields = FIELDS(
        FIELD(PUBLIC.STATIC, "result", ObjectType.String.toJVMTypeName), // Ljava/lang/String
        FIELD(PUBLIC.STATIC, "input", ArrayType(ObjectType.String).toJVMTypeName)
      ),
      methods = METHODS(
        METHOD(
          STATIC.PUBLIC,
          "<clinit>",
          MethodDescriptor.NoArgsAndReturnVoid.toJVMDescriptor,
          CODE(
            ICONST_0,
            ANEWARRAY(ObjectType.String),
            PUTSTATIC(thisType, "input", ArrayType(ObjectType.String).toJVMTypeName),
            RETURN
          )
        ),
        METHOD(
          STATIC.PUBLIC,
          "logInput",
          MethodDescriptor.JustTakes(ObjectType.String).toJVMDescriptor,
          CODE(
            GETSTATIC(thisType, "input", ArrayType(ObjectType.String).toJVMTypeName),
            ASTORE_3,
            ALOAD_3, // REG_3 = Old Array
            ARRAYLENGTH,
            ICONST_1,
            IADD,
            DUP,
            ISTORE_1, // REG_1 = New Length
            ANEWARRAY(ObjectType.String),
            ASTORE(4), // REG_4 = New Array
            ALOAD_3,
            ICONST_0,
            ALOAD(4),
            ICONST_0,
            ALOAD_3,
            ARRAYLENGTH,
            INVOKESTATIC("java/lang/System", isInterface = false, "arraycopy", MethodDescriptor(RefArray(ObjectType.Object, IntegerType, ObjectType.Object, IntegerType, IntegerType), VoidType).toJVMDescriptor),
            ALOAD(4),
            ALOAD(4),
            ALOAD_3,
            ARRAYLENGTH,
            ALOAD_0,
            AASTORE,
            PUTSTATIC(thisType, "input", ArrayType(ObjectType.String).toJVMTypeName),
            RETURN
          )
        ),
        METHOD(
          PUBLIC.STATIC,
          "logString",
          MethodDescriptor.JustTakes(ObjectType("java/lang/CharSequence")).toJVMDescriptor,
          CODE(
            ALOAD_0,
            INVOKEVIRTUAL(ObjectType.Object, "toString", MethodDescriptor.JustReturnsString),
            PUTSTATIC(thisType, "result", ObjectType.String.toJVMTypeName),
            RETURN
          )
        )
      )
    ).toBR._1
  }

}

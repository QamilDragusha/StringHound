package customBRClasses.leakers

import org.opalj.ba.{CLASS, CODE, FIELD, FIELDS, METHOD, METHODS, PUBLIC, STATIC}
import org.opalj.bi
import org.opalj.br.{ ClassFile, MethodDescriptor, ObjectType}
import org.opalj.br.instructions.{AASTORE, ALOAD, ALOAD_0, ALOAD_3, ANEWARRAY, ARRAYLENGTH, ASTORE, ASTORE_3, DUP, GETSTATIC, IADD, ICONST_0, ICONST_1, INVOKESTATIC, INVOKEVIRTUAL, ISTORE_1, PUTSTATIC, RETURN}

object ByteBufferLeaker extends Leaker {

  private def thisType = "slicing/ByteBufferLeaker"
  private def byteBufferType = ObjectType("java/nio/ByteBuffer")

  override def objectType: ObjectType = ObjectType(thisType)

  override def classFile: ClassFile = {
    CLASS(
      version = bi.Java5Version,
      accessModifiers = PUBLIC,
      thisType = thisType,
      fields = FIELDS(
        FIELD(PUBLIC.STATIC, "result", byteBufferType.toJVMTypeName),
        // qamil TODO: Ist das nicht unnötig? Validieren und ggf. löschen
        FIELD(PUBLIC.STATIC, "input", byteBufferType.toJVMTypeName)
      ),
      methods = METHODS(
        METHOD(
          STATIC.PUBLIC,
          "<clinit>",
          MethodDescriptor.NoArgsAndReturnVoid.toJVMDescriptor,
          CODE(
            ICONST_0,
            ANEWARRAY(ObjectType.String),
            PUTSTATIC(thisType, "input", byteBufferType.toJVMTypeName),
            RETURN
          )
        ),
        METHOD(
          PUBLIC.STATIC,
          "logByteBuffer",
          MethodDescriptor.JustTakes(byteBufferType).toJVMDescriptor,
          CODE(
            ALOAD_0,
            PUTSTATIC(thisType, "result", byteBufferType.toJVMTypeName),
            RETURN
          )
        )
      )
    ).toBR._1
  }

}

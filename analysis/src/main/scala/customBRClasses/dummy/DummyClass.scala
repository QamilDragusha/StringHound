package customBRClasses.dummy

import customBRClasses.CustomBRClass
import org.opalj.ba.{CLASS, CODE, FIELDS, METHOD, METHODS, PUBLIC}
import org.opalj.bi
import org.opalj.br.{ClassFile, MethodDescriptor, ObjectType}
import org.opalj.br.instructions.{ALOAD_0, INVOKESPECIAL, RETURN}

object DummyClass extends CustomBRClass {

  private def thisType = "slicing/DummyClass"

  override def objectType: ObjectType = ObjectType(thisType)

  override def classFile: ClassFile = {
    CLASS(
      version = bi.Java5Version,
      accessModifiers = PUBLIC,
      thisType = thisType,
      fields = FIELDS(),
      methods = METHODS(
        METHOD(
          PUBLIC,
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
        )
      )
    ).toBR._1
  }

}

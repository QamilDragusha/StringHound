package customBRClasses.androidLib

import customBRClasses.CustomBRClass
import customBRClasses.java.CBRInputStream
import org.opalj.ba.{CLASS, CODE, FIELDS, METHOD, METHODS, PUBLIC, STATIC}
import org.opalj.bi
import org.opalj.br.instructions.{ALOAD_0, DUP, INVOKESPECIAL, NEW, RETURN}
import org.opalj.br.{ClassFile, MethodDescriptor, ObjectType}

object AndroidAssetManager extends CustomBRClass {

  private def thisType = "android/content/res/AssetManager"

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
          "open",
          MethodDescriptor(ObjectType.String, CBRInputStream.objectType).toJVMDescriptor,
          CODE(
            NEW(CBRInputStream.objectType),
            DUP,
            INVOKESPECIAL(CBRInputStream.objectType,isInterface = false,"<init>",MethodDescriptor.NoArgsAndReturnVoid),
            RETURN,
          )
        ),
      ),
    ).toBR._1
  }

}
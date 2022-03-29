package customBRClasses.androidLib

import customBRClasses.CustomBRClass
import org.opalj.ba.{CLASS, CODE, FIELDS, METHOD, METHODS, PUBLIC, STATIC}
import org.opalj.bi
import org.opalj.br.instructions.{INVOKESPECIAL, NEW, RETURN}
import org.opalj.br.{ClassFile, MethodDescriptor, ObjectType}

object AndroidContext extends CustomBRClass {

  private def thisType = "android/content/Context";

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
            RETURN,
          )
        ),
        METHOD(
          PUBLIC,
          "getAssets",
          MethodDescriptor.withNoArgs(AndroidAssetManager.objectType).toJVMDescriptor,
          CODE(
            NEW(AndroidAssetManager.objectType),
            INVOKESPECIAL(AndroidAssetManager.objectType,false,"<init>",MethodDescriptor.NoArgsAndReturnVoid),
            RETURN,
          )
        ),
      ),
    ).toBR._1
  }

}

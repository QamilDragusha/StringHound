package customBRClasses

import org.opalj.br.{ClassFile, ObjectType}


trait CustomBRClass {

  def objectType : ObjectType
  def classFile : ClassFile

}

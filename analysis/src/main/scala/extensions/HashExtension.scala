package extensions

import scala.collection.mutable
import org.opalj.br.{ClassFile, Method, ObjectType}

object HashExtensions {
  implicit class HashExtension(val hm: mutable.HashMap[Method, Array[Int]]) {
    def concatTransitive(other: mutable.HashMap[Method, Array[Int]]) : mutable.HashMap[Method, Array[Int]] = {
      val newHm : mutable.HashMap[Method, Array[Int]] = hm.clone()
      other.foreach {
        methodAndPcs => {
          val (method, pcs) = methodAndPcs
          if (!newHm.contains(method)) {
            newHm += (method -> Array())
          }
          newHm(method) = newHm(method) ++ pcs
        }
      }
      newHm
    }
  }
}



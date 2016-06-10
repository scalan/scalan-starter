package scalan.examples

import scala.reflect.ClassTag
import scalan.Scalan

trait Helpers { scalan: Scalan =>
  def print[T](x: T): String = x match {
    case p: Product => p.productIterator.map(print(_)).mkString("(", ",", ")")
    case a: Array[_] => a.mkString("[", ",", "]")
    case _ => x.toString
  }
  def print[T](name: String, m: Array[Array[T]]): Unit = {
    val nameLine = m.length / 2
    val res = m.zipWithIndex.map { case (row, i) => {
        row.map(print(_)).mkString(if (i == nameLine) s"$name:\t" else "\t", "\t", "\t")
      }
    }.mkString("\n")
    println(res)
    println()
  }
  def printRow[T:ClassTag](name: String, v: Array[T]): Unit = print(name, Array(v))
  def printColumn[T:ClassTag](name: String, v: Array[T]): Unit = print(name, v.map(Array(_)))

  implicit class LambdaOpsForConv[A:Elem,B:Elem](f: Rep[A => B]) {
    def asConv: Conv[A,B] = baseConv(f)
  }
}

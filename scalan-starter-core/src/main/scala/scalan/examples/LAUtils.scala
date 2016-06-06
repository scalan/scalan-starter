package scalan.examples

import scala.reflect.ClassTag

trait LAUtils {
  def print[T](name: String, m: Array[Array[T]]): Unit = {
    val nameLine = m.length / 2
    val res = m.zipWithIndex.map { case (row, i) => {
        row.mkString(if (i == nameLine) s"$name:\t" else "\t", "\t", "\t")
      }
    }.mkString("\n")
    println(res)
    println()
  }
  def printRow[T:ClassTag](name: String, v: Array[T]): Unit = print(name, Array(v))
  def printColumn[T:ClassTag](name: String, v: Array[T]): Unit = print(name, v.map(Array(_)))
}

package scalan.examples

import scala.reflect.ClassTag
import scala.util.Random
import scalan.Scalan

object Helpers { //scalan: Scalan =>
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

  val rnd = new Random(1)

  def genArray(len: Int): Array[Double] = {
    { for (i <- 1 to len) yield { rnd.nextDouble() } }.toArray
  }

  def genMatr(rows: Int, cols: Int): Array[Array[Double]] = {
    { for (i <- 1 to rows) yield { genArray(cols) } }.toArray
  }

  def time[A](name: String)(block: => A) = {
    val start = System.nanoTime()
    val result = block
    val end = System.nanoTime()
    val t = (end - start).toDouble
    println(s"$name (${Console.BLUE}${(t / 1e6)}${Console.RESET} ms)")
    (result, t)
  }

}

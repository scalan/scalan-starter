package scalan.examples

import scala.reflect.ClassTag
import scalan._
import scalan.compilation.{KernelTypes, KernelStore}
import scalan.linalgebra.{LADslExp, LADslStd, LADsl}

trait Example2 extends Scalan with LADsl {

  lazy val mvm = fun { p: Rep[(Matrix[Double], Vector[Double])] =>
    val Pair(m, v) = p
    DenseVector(m.rows.mapBy( fun{ r => r dot v }))
  }

  lazy val ddmvmC = fun { p: Rep[(Collection[Collection[Double]], Collection[Double])] =>
    val Pair(m, v) = p
    val width = m(0).length
    val matrix: Matr[Double] = CompoundMatrix(m.map { r: Coll[Double] => DenseVector(r) }, width)
    val vector: Vec[Double] = DenseVector(v)
    mvm(matrix, vector).items
  }

  lazy val ddmvmA = fun { p: Rep[(Array[Array[Double]], Array[Double])] =>
    val Pair(m, v) = p
    val matrix = CollectionOverArray(m.map(r => CollectionOverArray(r)))
    val vector = CollectionOverArray(v)
    ddmvmC(matrix, vector).arr
  }
  val matrData = Array(
    Array(1.0, 0, 1.0),
    Array(0, 1.0, 0),
    Array(1.0, 0, 1.0)
  )
  val vecData = Array(1.0,2.0,3.0)
}

class Example2Std extends LADslStd with Example2

object Demo2Std extends App with LAUtils {
  // create standard evaluation context
  val ctx = new Example2Std
  import ctx._
  print("m", matrData)
  printColumn("v", vecData)
  val res = ddmvmA((matrData, vecData))
  printColumn("r", res)
}

class Example2Exp extends LADslExp with Example2 {
  var doInvoke = false
  override def invokeAll = doInvoke
}

object Demo2Exp extends App with LAUtils {
  // create staged evaluation context
  val ctxExp = new Example2Exp
  ctxExp.ddmvmA.show

  val kstore = KernelStore.open(ctxExp, KernelTypes.ScalaKernel)
  import kstore.scalan._
  val ddmvmA_k = kstore.createKernel("ddmvmA", ddmvmA)

  print("m", matrData)
  printColumn("v", vecData)
  val res = ddmvmA_k((matrData, vecData))
  printColumn("r", res)
}

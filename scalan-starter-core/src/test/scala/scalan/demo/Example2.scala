package scalan.demo

import java.io.File

import scalan.{Scalan, JNIExtractorOpsExp}
import scalan.compilation.{KernelStore, KernelType}
import scalan.examples.Helpers
import scalan.linalgebra.{LADslExp, LADslStd, LADsl}

trait Example2 extends Scalan with LADsl with Helpers {

  lazy val mvm = fun { p: Rep[(Matrix[Double], Vector[Double])] =>
    val Pair(m, v) = p
    DenseVector(m.rows.map { r => r dot v })
  }

  lazy val dmdvC = fun { p: Rep[(Collection[Collection[Double]], Collection[Double])] =>
    val Pair(m_data, v_data) = p
    val m = DenseMatrix(m_data)
    val v = DenseVector(v_data)
    mvm(m, v).items
  }

  lazy val dmdvA = fun { p: Rep[(Array[Array[Double]], Array[Double])] =>
    val Pair(m_data, v_data) = p
    val m = CompoundCollection(m_data)
    val v = Collection(v_data)
    dmdvC((m, v)).arr
  }

  val dmData = Array(
    Array(1.0, 0, 1.0),
    Array(0, 1.0, 0),
    Array(1.0, 0, 1.0)
  )
  val dvData = Array(1.0,2.0,3.0)
}

class Example2Std extends LADslStd with Example2

class Example2Exp extends LADslExp with JNIExtractorOpsExp with Example2 {
  var doInvoke = true
  override def invokeAll = doInvoke
}


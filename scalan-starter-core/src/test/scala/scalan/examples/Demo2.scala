package scalan.examples

import scala.reflect.ClassTag
import scalan._
import scalan.compilation.{KernelTypes, KernelStore}
import scalan.linalgebra.{LADslExp, LADslStd, LADsl}

trait Example2 extends Scalan with LADsl with LAUtils {

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
  val smData = Array(
    Array((0,1.0), (2,1.0)),
    Array((1,1.0)),
    Array((0,1.0), (2, 1.0))
  )
  val dvData = Array(1.0,2.0,3.0)
}

class Example2Std extends LADslStd with Example2

object Demo2Std extends Example2Std with App {

  val res = dmdvA((dmData, dvData))

  print("m", dmData)
  printColumn("v", dvData)
  printColumn("r", res)
}

class Example2Exp extends LADslExp with Example2 {
  var doInvoke = true
  override def invokeAll = doInvoke
}

object Demo2Exp extends Example2Exp with App {
  mvm.show
  dmdvC.show
  dmdvA.show

  val kstore = KernelStore.open(this, KernelTypes.ScalaKernel)
  import kstore.scalan._

  val dmdvA_k = kstore.createKernel("dmdvA", dmdvA)

  val res = dmdvA_k((dmData, dvData))

  print("m", dmData)
  printColumn("v", dvData)
  printColumn("r", res)
}

package scalan.examples

import java.io.File

import scalan.Scalan
import scalan.compilation.{KernelStore, KernelType}
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
  val smData = Array(
    Array((0,1.0), (2,1.0)),
    Array((1,1.0)),
    Array((0,1.0), (2, 1.0))
  )
  val dvData = Array(1.0,2.0,3.0)
}

class Example2Std extends LADslStd with Example2

object Demo2Std extends App {
  val ctx = new Example2Std
  import ctx._
  val res = dmdvA((dmData, dvData))

  print("m", dmData)
  printColumn("v", dvData)
  printColumn("r", res)
}

class Example2Exp extends LADslExp with Example2 {
  var doInvoke = true
  override def invokeAll = doInvoke
}

object Demo2Exp {
  def main(args: Array[String]): Unit = {
    val ctx = new Example2Exp
    import ctx._
    //  mvm.show
    //  dmdvC.show
    //  dmdvA.show

    val kernelsDir = new File("./test-out/Demo2Exp")
    val kstore = KernelStore.open(ctx, kernelsDir)

    val dmdvA_k = kstore.createKernel("dmdvA", KernelType.Scala, dmdvA)

    val res = dmdvA_k((dmData, dvData))

    print("m", dmData)
    printColumn("v", dvData)
    printColumn("r", res)
  }
}

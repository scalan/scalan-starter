package scalan.examples

import java.io.File

import scalan._
import scalan.compilation.{KernelStore, KernelType}
import scalan.linalgebra.{LADslExp, LADslStd, LADsl}

trait Example3 extends Scalan with LADsl with LAUtils  {

  lazy val mvm = fun { p: Rep[(Matrix[Double], Vector[Double])] =>
    val Pair(m, v) = p
    DenseVector(m.rows.map { r => r dot v })
  }

  val Some(sparse2dense) = getConverter(element[SparseMatrix[Double]], element[DenseMatrix[Double]])

  lazy val smdvA = fun { p: Rep[((Array[(Array[Int],Array[Double])], Int), Array[Double])] =>
    val Pair(Pair(m_data, nCols), v_data) = p
    val m = sparse2dense(SparseMatrix(m_data, nCols))
    val v = DenseVector(v_data)
    mvm((m, v)).items.arr
  }

  val smData = (Array(
    (Array(0,2), Array(1.0,1.0)),
    (Array(1), Array(1.0)),
    (Array(0, 2), Array(1.0, 1.0))
    ), 3)
  val dvData = Array(1.0,2.0,3.0)
}

class Example3Std extends LADslStd with Example3

object Demo3Std extends App {
  val ctx = new Example3Std
  import ctx._
  val res = smdvA((smData, dvData))

  printColumn("sm", smData._1)
  printColumn("dv", dvData)
  printColumn("dr", res)
}

class Example3Exp extends LADslExp with Example3 {
  var doInvoke = true
  override def invokeAll = doInvoke
  override def rewriteDef[T](d: Def[T]) = d match {
    case ArrayMap(ys @ Def(d2), f: RFunc[a, b]@unchecked) =>
      d2.asDef[Array[a]] match {
        case ArrayMap(xs: Rep[Array[c]]@unchecked, g) => //TODO if hasSingleUsage(ys)
          val xs1 = xs.asRep[Array[c]]
          val g1 = g.asRep[c => a]
          implicit val eB = f.elem.eRange
          implicit val eC = xs.elem.eItem
          val res = xs1.map { x => f(g1(x))}
          res
        case _ =>
          super.rewriteDef(d)
      }
    case _ =>
      super.rewriteDef(d)
  }
}

object Demo3Exp extends App {
  val ctx = new Example3Exp
  import ctx._
//  mvm.show
//  sparse2dense.show
//  smdvA.show

  val kernelsDir = new File("./test-out/Demo3Exp")
  val kstore = KernelStore.open(ctx, kernelsDir)

  val smdvA_k = kstore.createKernel("smdvA", KernelType.Scala, smdvA)

  val res = smdvA_k((smData, dvData))

  printColumn("m", smData._1)
  printColumn("v", dvData)
  printColumn("r", res)
}

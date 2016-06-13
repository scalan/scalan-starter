package scalan.demo

import scalan.{Scalan, JNIExtractorOpsExp}
import scalan.examples.Helpers._
import scalan.linalgebra.{LADslExp, LADslStd, LADsl}

trait Example3 extends Scalan with LADsl {

  lazy val mvm = fun { p: Rep[(Matrix[Double], Vector[Double])] =>
    val Pair(m, v) = p
    DenseVector(m.rows.map { r => r dot v })
  }

  val Some(conv) = getConverter(
    element[(SparseMatrix[Double],Vector[Double])],
    element[(DenseMatrix[Double], Vector[Double])])

//  val Some(sparse2dense) = getConverter(
//    element[SparseMatrix[Double]], element[DenseMatrix[Double]])
//  val conv = pairConv(sparse2dense, identityConv[Vector[Double]])

  lazy val smdvA = fun { p: Rep[((Array[(Array[Int],Array[Double])], Int), Array[Double])] =>
    val Pair(Pair(m_data, nCols), v_data) = p
    val sm = SparseMatrix(m_data, nCols)
    val dv = DenseVector(v_data)
    (conv >> mvm).apply(sm, dv).items.arr
  }

  val smData = (Array(
    (Array(0,2), Array(1.0,1.0)),
    (Array(1), Array(1.0)),
    (Array(0, 2), Array(1.0, 1.0))
    ), 3)
  val dvData = Array(1.0,2.0,3.0)
}

class Example3Std extends LADslStd with Example3

class Example3Exp extends LADslExp with JNIExtractorOpsExp with Example3 {
  var doInvoke = true
  override def invokeAll = doInvoke
  override def rewriteDef[T](d: Def[T]) = d match {
    case ArrayMap(ys @ Def(d2), f: RFunc[a, b]@unchecked) =>
      d2.asDef[Array[a]] match {
        case ArrayMap(xs: Rep[Array[c]]@unchecked, g) => //TODO if hasSingleUsage(ys)
          val g1 = g.asRep[c => a]
          implicit val eB = f.elem.eRange
          implicit val eC = xs.elem.eItem
          val res = xs.map { x => f(g1(x))}
          res
        case _ =>
          super.rewriteDef(d)
      }
    case _ =>
      super.rewriteDef(d)
  }
}


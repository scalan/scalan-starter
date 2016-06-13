package scalan.demo

import java.io.File

import scalan.{Scalan, JNIExtractorOpsExp}
import scalan.linalgebra.{LADslExp, LADslStd, LADsl}

trait ExampleMmm extends Scalan with LADsl {

  lazy val mvm = fun { p: Rep[(Matrix[Double], Vector[Double])] =>
    val Pair(m, v) = p
    DenseVector(m.rows.map { r => r dot v })
  }

  // tr(C) = A * tr(B) - multiply A to the transpose of B to get transpose of C
  lazy val mmm = fun { in: Rep[(Matrix[Double], Matrix[Double])] =>
    val Pair(a, bT) = in
    val columns = bT.rows  // each row of bT is the column of B
    val cT = columns.map { c => mvm(a, c).items }
    DenseMatrix(cT)
  }

  lazy val dmdmA = fun { in: Rep[(Array[Array[Double]], Array[Array[Double]])] =>
    val Pair(a_data, bT_data) = in
    val a = DenseMatrix(JuggedCollection(a_data))
    val bT = DenseMatrix(JuggedCollection(bT_data))
    mmm((a, bT)).toNColl.arr.map(_.arr)
  }

  val dmData = Array(
    Array(1.0, 0, 1.0),
    Array(0, 1.0, 0),
    Array(1.0, 0, 1.0)
  )
}

class ExampleMmmStd extends LADslStd with ExampleMmm

class ExampleMmmExp extends LADslExp with JNIExtractorOpsExp with ExampleMmm {
  var doInvoke = true
  override def invokeAll = doInvoke
}


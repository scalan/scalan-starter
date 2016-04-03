package scalan.examples

import scala.language.reflectiveCalls
import scalan.BaseViewTests

/**
  * A suite of tests which can also be used as snippets in Scala Console, thus all imports a local
  */
class ExampleStagingTests extends BaseViewTests {

  test("c1") {
    import scalan._
    import scalan.linalgebra._
    class Ctx extends LADslExp { override def invokeAll = true }
    val ctx = new Ctx
    import ctx._
    val sparse2dense = fun { v: Rep[SparseVector[Double]] =>
      DenseVector(v.items)
    }
    stage(ctx)(currentTestName, "sparse2dense_vector", Seq(() => sparse2dense))
  }

  test("c2") {
    import scalan._
    import scalan.linalgebra._
    var doInvoke = true
    class Ctx extends LADslExp { override def invokeAll = false }
    val ctx = new Ctx
    import ctx._
    def sparseData2denseData = fun { data: Rep[SparseVectorData[Double]] =>
      val v = SparseVector(data)
      DenseVector(v.items).toData
    }
    stage(ctx)(currentTestName, "sparse2dense_data_noInvoke", Seq(() => sparseData2denseData))
  }

  test("c3") {
    import scalan._
    import scalan.linalgebra._
    var doInvoke = true
    class Ctx extends LADslExp { override def invokeAll = true }
    val ctx = new Ctx
    import ctx._
    def sparseData2denseData = fun { data: Rep[SparseVectorData[Double]] =>
      val v = SparseVector(data)
      DenseVector(v.items).toData
    }
    stage(ctx)(currentTestName, "sparse2dense_data_invoke", Seq(() => sparseData2denseData))
  }

  test("c4") {
    import scalan._
    import scalan.linalgebra._
    class Ctx extends LADslExp { override def invokeAll = true }
    val ctx = new Ctx
    import ctx._
    val vector2dense = fun { v: Rep[Vector[Double]] =>
      DenseVector(v.items)
    }
    val sparseData2denseData = fun { data: Rep[SparseVectorData[Double]] =>
      val v = SparseVector(data)
      vector2dense(v).toData
    }
    val constData2denseData = fun { data: Rep[ConstVectorData[Double]] =>
      val v = ConstVector(data)
      vector2dense(v).toData
    }
    stage(ctx)(currentTestName, "two_converters",
               Seq(() => sparseData2denseData, () => constData2denseData ))
  }

  test("c5") {
    import scalan._
    import scalan.linalgebra._
    class Ctx extends LADslExp { override def invokeAll = true }
    val ctx = new Ctx
    import ctx._
    val Some(c) = hasConverter(element[Array[SparseVector[Double]]], element[Array[DenseVector[Double]]])
    val sparseData2denseData = fun { data: Rep[Array[SparseVectorData[Double]]] =>
      val vs = data.map(SparseVector(_))
      c(vs).map(_.toData)
    }
    stage(ctx)(currentTestName, "hasConverter_sparse_dense",
               Seq(() => sparseData2denseData))
  }

  test("isoSpec1") {
    import scalan._
    import scalan.linalgebra._
    var doInvoke = true
    class Ctx extends LADslExp { override def invokeAll = doInvoke }
    val ctx = new Ctx
    import ctx._

    lazy val mvm = fun { p: Rep[(Matrix[Double], Vector[Double])] =>
      val Pair(m, v) = p
      DenseVector(m.rows.mapBy( fun{ r => r dot v }))
    }
    stage(ctx)(currentTestName, "mvm",
      Seq(() => mvm))

    lazy val ddmvmC = fun { p: Rep[(Collection[Collection[Double]], Collection[Double])] =>
      val Pair(m, v) = p
      val width = m(0).length
      val matrix: Matr[Double] = CompoundMatrix(m.map { r: Coll[Double] => DenseVector(r) }, width)
      val vector: Vec[Double] = DenseVector(v)
      mvm(matrix, vector).items
    }
    stage(ctx)(currentTestName, "ddmvmC",
      Seq(() => ddmvmC))

    def ddmvmA = fun { p: Rep[(Array[Array[Double]], Array[Double])] =>
      val Pair(m, v) = p
      val matrix = CollectionOverArray(m.map(r => CollectionOverArray(r)))
      val vector = CollectionOverArray(v)
      ddmvmC(matrix, vector).arr
    }
    doInvoke = false
    stage(ctx)(currentTestName, "ddmvmA_noinvoke",
      Seq(() => ddmvmA))
    doInvoke = true
    stage(ctx)(currentTestName, "ddmvmA_invoke",
      Seq(() => ddmvmA))

    def ddmvmL = fun { p: Rep[(List[List[Double]], List[Double])] =>
      val Pair(m, v) = p
      val matrix = CollectionOverList(m.map(r => CollectionOverList(r)))
      val vector = CollectionOverList(v)
      ddmvmC(matrix, vector).arr
    }
    doInvoke = true
    stage(ctx)(currentTestName, "ddmvmL_invoke",
      Seq(() => ddmvmL))

  }

}

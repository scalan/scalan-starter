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
    class Ctx extends MatricesDslExp { override def invokeAll = true }
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
    class Ctx extends MatricesDslExp { override def invokeAll = false }
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
    class Ctx extends MatricesDslExp { override def invokeAll = true }
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
    class Ctx extends MatricesDslExp { override def invokeAll = true }
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
    class Ctx extends MatricesDslExp { override def invokeAll = true }
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
}

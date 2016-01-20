package scalan.examples

import scala.language.reflectiveCalls
import scalan.BaseViewTests

class ExampleStagingTests extends BaseViewTests {

  test("matrix_convertion") {
    import java.lang.reflect.Method
    import scalan._
    import scalan.collections._
    import scalan.linalgebra._
    var doInvoke = true
    class Ctx extends MatricesDslExp { override def isInvokeEnabled(d: Def[_], m: Method) = doInvoke }
    val ctx = new Ctx
    import ctx._
    val sparse2dense = fun { v: Rep[SparseVector[Double]] =>
      DenseVector(v.items)
    }
    def sparseData2denseData = fun { data: Rep[SparseVectorData[Double]] =>
      val v = SparseVector(data)
      DenseVector(v.items).toData
    }
    stage(ctx)(currentTestName, "sparse2dense", Seq(() => sparse2dense))
//    doInvoke = false
//    stage(ctx)(currentTestName, "sparseData2denseDataNoInvoke", Seq(() => sparseData2denseData))
    doInvoke = true
    stage(ctx)(currentTestName, "sparseData2denseData", Seq(() => sparseData2denseData))
//    showGraphs(sparse2dense)
  }

}

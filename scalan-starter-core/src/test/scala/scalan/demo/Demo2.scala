package scalan.demo

import java.io.File
import scalan.BaseNestedTests
import scalan.compilation.{KernelStore, KernelType}
import scalan.examples.Helpers._

class Demo2 extends BaseNestedTests {
  describe("Matrix Vector multiplication (mvm)") {

    val ctxStd = new Example2Std
    import ctxStd._
    it("Standard evaluation") {
      val res = dmdvA((dmData, dvData))

      print("m", dmData)
      printColumn("v", dvData)
      printColumn("r", res)
    }

    it("Staged evaluation") {
      val ctx = new Example2Exp
      import ctx._

      val kernelsDir = new File("./test-out/Demo2/mvm")
      val store = KernelStore.open(ctx, kernelsDir)

      val (dmdvA_s, _) = time("Generating Scala kernel") {
        val k = store.createKernel("dmdvA", KernelType.Scala, dmdvA)
        k((dmData, dvData))
        k
      }

      val (dmdvA_c, _) = time("Generating C++ kernel") {
        val k = store.createKernel("dmdvA", KernelType.Cpp, dmdvA)
        k((dmData, dvData))
        k
      }

      val rows = 10000
      val cols = rows;

      val (m,_) = time( s"""Generating ${rows}x${cols} Array[Array[Double]] ...""") {
        genMatr(rows, cols)
      }

      val (v, _) = time( s"""Generating ${rows} Array[Double] ...""") {
        genArray(cols)
      }

      val (correctRes,_) = time("Calculating expected result...") {
        m.map { r: Array[Double] =>
           r.zip(v)
            .map { case (x,y) => x * y }
            .reduce(_ + _)
        }
      }

      val (resStd,_) = time(s"Standard Evaluation") { ctxStd.dmdvA((m, v)) }
      val (resS,_) = time(s"Executing Scala kernel") { dmdvA_s((m, v)) }
      val (resC,_) = time(s"Executing C++ kernel") { dmdvA_c((m, v)) }

      assertResult(correctRes)(resStd)
      assertResult(correctRes)(resS)
      assertResult(correctRes)(resC)
    }

  }

}



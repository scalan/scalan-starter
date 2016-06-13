package scalan.demo

import java.io.File
import scalan.BaseNestedTests
import scalan.compilation.{KernelStore, KernelType}
import scalan.examples.Helpers._

class Demo2 extends BaseNestedTests {

  describe("Matrix Vector multiplication (mvm)") {
    it("Standard evaluation") {
      val ctx = new Example2Std
      import ctx._
      val res = dmdvA((dmData, dvData))

      print("m", dmData)
      printColumn("v", dvData)
      printColumn("r", res)
    }

    it("Staged evaluation") {
      val ctx = new Example2Exp
      import ctx._

      val kernelsDir = new File("./test-out/Demo2Exp/mvm")
      val kstore = KernelStore.open(ctx, kernelsDir)

      val (dmdvA_s, _) = time("Generating Scala kernel") {
        val k = kstore.createKernel("dmdvA", KernelType.Scala, dmdvA)
        k((dmData, dvData))
        k
      }

      val (dmdvA_c, _) = time("Generating C++ kernel") {
        val k = kstore.createKernel("dmdvA", KernelType.Cpp, dmdvA)
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

      val (correctRes, t1) = time("Calculating expected result...") {
        m.map({ r: Array[Double] =>
           r.zip(v)
            .map({ p => p._1 * p._2 })
            .reduce(_ + _)
        })
      }

      val (resS,_) = time(s"Executing Scala kernel") { dmdvA_s((m, v)) }
      val (resC,_) = time(s"Executing C++ kernel") { dmdvA_c((m, v)) }

      assertResult(correctRes)(resS)
      assertResult(correctRes)(resC)
    }

  }

}



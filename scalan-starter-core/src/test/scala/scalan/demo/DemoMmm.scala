package scalan.demo

import java.io.File
import scalan.BaseNestedTests
import scalan.compilation.{KernelStore, KernelType}
import scalan.examples.Helpers._

class DemoMmm extends BaseNestedTests {

  describe("Matrix Matrix multiplication (mmm)") {
    it("Standard evaluation") {
      val ctx = new ExampleMmmStd
      import ctx._
      val res = dmdmA((dmData, dmData))

      print("m", dmData)
      print("m", dmData)
      print("r", res)
    }

    describe("Staged evaluation") {
      val ctx = new ExampleMmmExp
      import ctx._

      val kernelsDir = new File("./test-out/Demo2Exp/mmm")
      val kstore = KernelStore.open(ctx, kernelsDir)

      val rows = 500
      val cols = rows

      val (a,_) = time( s"""Generating first ${rows}x${cols} Array[Array[Double]] ...""") {
        genMatr(rows, cols)
      }

      val (bT, _) = time( s"""Generating second ${rows}x${cols} Array[Array[Double]] ...""") {
        genMatr(rows, cols)
      }

      val (correctRes, t1) = time("Calculating expected result...") {
        bT.map { bColumn =>
          a.map { aRow =>
            aRow.zip(bColumn)
              .map { case (x,y) => x * y }
              .sum
          }
        }
      }

      it("Scala") {
        val (dmdmA_s, _) = time("Generating Scala kernel") {
          val k = kstore.createKernel("dmdmA", KernelType.Scala, dmdmA)
          k((dmData, dmData))
          k
        }

        val (resS,_) = time(s"Executing Scala kernel") { dmdmA_s((a, bT)) }

        assertResult(correctRes)(resS)
      }

      // generated C++ code is currently incorrect
      ignore("C++") {
        val (dmdmA_c, _) = time("Generating C++ kernel") {
          val k = kstore.createKernel("dmdmA", KernelType.Cpp, dmdmA)
          k((dmData, dmData))
          k
        }

        val (resC, _) = time(s"Executing C++ kernel") {dmdmA_c((a, bT))}

        assertResult(correctRes)(resC)
      }
    }
  }
}

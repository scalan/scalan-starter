package scalan.demo

import java.io.File

import scalan.BaseNestedTests
import scalan.compilation.{KernelStore, KernelType}

class Demo2 extends BaseNestedTests {

  describe("Composition of Converter with mvm") {
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

      val kernelsDir = new File("./test-out/Demo2Exp")
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

      val (inM,_) = time( s"""Generating random matrix ${rows}x${cols} dense format "Array[Array[Double]]" ...""") {
        genMatr(rows, cols)
      }

      val (inV, _) = time( s"""Generating random vector ${rows} elements "Array[Double]" ...""") {
        genArray(cols)
      }

      val (correctRes, t1) = time("Calculating expected result...") {
        val correctRes = inM.map({ r: Array[Double] => r.zip(inV).map({ p => p._1 * p._2 }).fold(0.0)(_ + _) })
        correctRes
      }

      val (resS,_) = time(s"Executing Scala kernel") { dmdvA_s((inM, inV)) }
      val (resC,_) = time(s"Executing C++ kernel") { dmdvA_c((inM, inV)) }

//      print("m", dmData)
//      printColumn("v", dvData)
//      printColumn("r", res)
    }

  }
}

package scalan.demo

import java.io.File

import scalan.BaseNestedTests
import scalan.compilation.{KernelStore, KernelType}
import scalan.examples.Helpers._

class Demo4 extends BaseNestedTests {
  val env = (0,"abc")

  describe("Monads") {
    it("Standard evaluation") {
      val ctx = new Example4Std
      import ctx._

      val n = 1000
      val (resId,_) = time(s"sumWithId ${n} items") { sumWithId(n) }
      val (resReader,_) = time(s"sumWithReader ${n} items") { sumWithReader((env,n)) }

      assertResult(resId)(resReader)
    }

    describe("Staged evaluation") {
      val kernelsDir = new File("./test-out/Demo4")
      val ctx = new Example4Exp { }
      val kstore = KernelStore.open(ctx, kernelsDir)
      import ctx._

      val n = 10000
      it("identityMonad") {
        val (sum, _) = time("Generating Scala kernel") {
          val k = kstore.createKernel("sumWithId", KernelType.Scala, sumWithId)
          k(10)
          k
        }
        val (res,_) = time (s"sumWithId ${n} items") { sum(n) }
      }

      it("readerMonad") {
        val (sum, _) = time("Generating Scala kernel") {
          val k = kstore.createKernel("sumWithReader", KernelType.Scala, sumWithReader)
          k((env, 10))
          k
        }

        val res = time (s"sumWithReader ${n} items") { sum((env, n)) }
      }
    }
  }
}



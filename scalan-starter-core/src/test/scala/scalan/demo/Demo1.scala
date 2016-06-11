package scalan.demo

import java.io.File

import scalan.{BaseNestedTests, JNIExtractorOpsExp}
import scalan.compilation.{KernelStore, KernelType}
import scalan.linalgebra.LADslExp

class Demo1 extends BaseNestedTests {

  describe("Matrix Vector multiplication (mvm)") {
    it("Standard evaluation") {
      // create standard evaluation context
      val ctx = new ExampleStd
      ctx.run
    }

    it("Staged evaluation") {
      // create staged evaluation context
      val ctxExp = new ExampleExp
      ctxExp.run
    }

    it("Kernels Demo") {
      val ctx        = new ExampleExp with LADslExp with JNIExtractorOpsExp
      val kernelsDir = new File("./test-out/KernelsDemo")
      val store      = KernelStore.open(ctx, kernelsDir)
      val plusS      = store.createKernel("plus", KernelType.Scala, ctx.plusFun)
      val resS       = plusS((10, 20))

      val plusCp = store.createKernel("plus", KernelType.Cpp, ctx.plusFun)
      val resC = plusCp((10, 20))
      assert(resS == resC)
    }
  }

}

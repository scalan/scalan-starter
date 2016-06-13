package scalan.demo

import java.io.File

import scalan.{BaseNestedTests, JNIExtractorOpsExp}
import scalan.compilation.{KernelStore, KernelType}
import scalan.examples.Helpers._
import scalan.linalgebra.LADslExp

class Demo1 extends BaseNestedTests {

  describe("Code Virtualization") {

    val ctxStd = new ExampleStd
    it("supports Standard Evaluation") {
      // create standard evaluation context
      ctxStd.run
    }

    it("supports Staged Evaluation") {
      // create staged evaluation context
      val ctxExp = new ExampleExp
      ctxExp.run
    }

    it("compiles kernels equivalent to standard evaluation") {
      val ctx        = new ExampleExp with JNIExtractorOpsExp
      val kernelsDir = new File("./test-out/Demo1")
      val store      = KernelStore.open(ctx, kernelsDir)
      val data       = (ctxStd.x, ctxStd.y)
      val resStd     = ctxStd.plusFun(data)

      val (plusS,_)  = time ("Generating Scala kernel") {
        store.createKernel("plus", KernelType.Scala, ctx.plusFun)
      }
      val resS       = plusS(data)

      val (plusC,_)  = time ("Generating C++ kernel") {
        store.createKernel("plus", KernelType.Cpp, ctx.plusFun)
      }
      val resC       = plusC(data)

      assertResult(resStd)(resS)
      assertResult(resStd)(resC)
    }
  }

}

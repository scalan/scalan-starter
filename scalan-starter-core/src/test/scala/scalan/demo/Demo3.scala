package scalan.demo

import java.io.File
import scalan.{BaseNestedTests}
import scalan.compilation.{KernelStore, KernelType}

class Demo3 extends BaseNestedTests {

  describe("Composition of Converter with mvm") {
    it("Standard evaluation") {
      val ctx = new Example3Std
      import ctx._
      val res = smdvA((smData, dvData))

      printColumn("sm", smData._1)
      printColumn("dv", dvData)
      printColumn("dr", res)
    }

    it("Staged evaluation") {
      val ctx = new Example3Exp
      import ctx._

      val kernelsDir = new File("./test-out/Demo3Exp")
      val kstore = KernelStore.open(ctx, kernelsDir)

      val smdvA_s = kstore.createKernel("smdvA", KernelType.Scala, smdvA)
      val smdvA_c = kstore.createKernel("smdvA", KernelType.Cpp, smdvA)

      val resS = smdvA_s((smData, dvData))
      val resC = smdvA_c((smData, dvData))

      printColumn("m", smData._1)
      printColumn("v", dvData)
      printColumn("rS", resS)
      printColumn("rC", resC)
    }

  }
}

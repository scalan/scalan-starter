package scalan.examples

import java.io.File

import scalan.{ScalanDslExp, ScalanDslStd, Scalan, JNIExtractorOpsExp}
import scalan.compilation.{KernelStore, KernelType}
import scalan.linalgebra.LADslExp
import scalan.util.FileUtil

/**
  * Example code that can be executed both in standard evaluation
  * and staged evaluation contexts
  */
trait Example extends Scalan {
  lazy val x: Rep[Int] = 10
  lazy val y = x + 1
  val plus = { in: Rep[(Int, Int)] =>
    val Pair(x, y) = in
    x + y
  }
  lazy val plusFun = mkLambda(plus, mayInline = false)
  def run = {
    val z = plusFun((x,y))
    println(s"plus($x, $y) = $z")
  }
}

class ExampleStd extends ScalanDslStd with Example

object DemoStd extends App {
  // create standard evaluation context
  val ctx = new ExampleStd
  ctx.run
}

class ExampleExp extends ScalanDslExp with Example {
  override val currentPass = new DefaultPass(
    "mypass", Pass.defaultPassConfig.copy(constantPropagation = false))
}

object DemoExp extends App {
  // create staged evaluation context
  val ctxExp = new ExampleExp
  ctxExp.run
}

object KernelsDemo extends App{
  val ctx        = new ExampleExp with LADslExp with JNIExtractorOpsExp
  val kernelsDir = new File("./test-out/KernelsDemo")
  val store      = KernelStore.open(ctx, kernelsDir)
  val plusS      = store.createKernel("plus", KernelType.Scala, ctx.plusFun)
  val resS       = plusS((10, 20))

  val plusCp = store.createKernel("plus", KernelType.Cpp, ctx.plusFun)
  val resC = plusCp((10, 20))
  assert(resS == resC)
}

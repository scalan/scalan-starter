package scalan.examples

import scalan._
import scalan.compilation.{KernelType, KernelStore}

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

object KernelsDemo {
  val ctx = new ExampleExp
  val scalaStore = KernelStore.open(ctx, KernelType.Scala)
  val plusS = scalaStore.createKernel("plus", scalaStore.scalan.plusFun)
  val resS = plusS((10, 20))

  val cppStore = KernelStore.open(ctx, KernelType.Cpp)
  val plusCp = cppStore.createKernel("plus", cppStore.scalan.plusFun)
  val resC = plusCp((10, 20))
  assert(resS == resC)
}

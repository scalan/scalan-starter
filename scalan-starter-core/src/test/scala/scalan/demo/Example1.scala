package scalan.demo

import scalan.{ScalanDslExp, ScalanDslStd, Scalan}

/**
  * Example code that can be executed both in standard evaluation
  * and staged evaluation contexts.
  */
trait Example extends Scalan { // abstract context
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

class ExampleExp extends ScalanDslExp with Example {
  override val currentPass = new DefaultPass(
    "mypass", Pass.defaultPassConfig.copy(constantPropagation = false))
}


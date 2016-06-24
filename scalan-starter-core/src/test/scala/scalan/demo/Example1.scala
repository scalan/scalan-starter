package scalan.demo

import scalan.{ScalanDslExp, ScalanDslStd, Scalan, ScalanDsl}

/**
  * Example code that can be executed both in standard evaluation
  * and staged evaluation contexts.
  */
trait Example1 extends ScalanDsl { // abstract context
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

class Example1Std extends ScalanDslStd with Example1 { // standard context
  val someVal: Rep[Int] = 10
}

class Example1Exp extends ScalanDslExp with Example1 { // staged context
  override val currentPass = new DefaultPass(
    "mypass", Pass.defaultPassConfig.copy(constantPropagation = false))
  val someVal: Rep[Int] = 10
}


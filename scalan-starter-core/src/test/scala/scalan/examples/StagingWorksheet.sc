//-------------------
import scalan._
val ctx = new ScalanDslExp {
  override val currentPass = DefaultPass("mypass", Pass.defaultPassConfig.copy(constantPropagation = false))
}
import ctx._
val x: Rep[Int] = 10
val y = x + 1

val y = inc(x)
val y1 = ProgramGraph.transform(y)
showGraphs(y, y1)

//-------------------
import scalan._
val ctx = new ScalanDslExp
import ctx._
val calc = fun { (in: Rep[(Int, (Int, Int))]) =>
  val Pair(a, Pair(b, c)) = in
  a * c + b * c
}

def lemma = postulate {
  (a: Rep[Int], b: Rep[Int], c: Rep[Int]) =>
    a * c + b * c  <=> (a + b) * c
}
val rw = new RulesRewriter(patternRewriteRule(lemma))
val calcOpt = ProgramGraph.transform(calc, rw)
showGraphs(calc, calcOpt)

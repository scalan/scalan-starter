package scalan

import scalan.linalgebra.{LADslExp, LinearAlgebraExamples}

class ScalanCake extends ScalanDslExp
  with LinearAlgebraExamples with LADslExp {
  override val cacheElems = false
  implicit val config = defaultGraphVizConfig
}

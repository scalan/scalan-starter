package scalan.examples

import scalan.ScalanDslExp
import scalan.linalgebra.{LADslExp, LinearAlgebraExamples}

class ScalanCake extends ScalanDslExp
  with ExampleDslExp with LinearAlgebraExamples with LADslExp {
  override val cacheElems = false
}

package scalan.examples

import scalan.ScalanDslExp
import scalan.linalgebra.{MatricesDslExp, LinearAlgebraExamples}

class ScalanCake extends ScalanDslExp
  with ExampleDslExp with LinearAlgebraExamples with MatricesDslExp {
  override val cacheElems = false
}

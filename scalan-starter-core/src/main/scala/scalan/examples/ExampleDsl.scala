package scalan.examples

import scalan.collections.{CollectionsDslExp, CollectionsDslSeq, CollectionsDsl}
import scalan.{ScalanDsl, ScalanDslSeq, ScalanDslExp}
import scalan.linalgebra._

/**
 * Created by Viktor Smirnov on 29.03.15.
 */
trait ExampleDsl extends ScalanDsl with MyArraysDsl with CollectionsDsl {

}

trait ExampleDslSeq extends ScalanDslSeq with ExampleDsl with MyArraysDslSeq with CollectionsDslSeq

trait ExampleDslExp extends ScalanDslExp with ExampleDsl with MyArraysDslExp with CollectionsDslExp

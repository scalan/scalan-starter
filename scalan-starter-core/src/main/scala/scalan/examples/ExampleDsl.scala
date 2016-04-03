package scalan.examples

import scalan.collections.{CollectionsDslExp, CollectionsDslStd, CollectionsDsl}
import scalan.{ScalanDsl, ScalanDslStd, ScalanDslExp}

trait ExampleDsl extends ScalanDsl with CollectionsDsl {

}

trait ExampleDslStd extends ScalanDslStd with ExampleDsl with CollectionsDslStd

trait ExampleDslExp extends ScalanDslExp with ExampleDsl with CollectionsDslExp

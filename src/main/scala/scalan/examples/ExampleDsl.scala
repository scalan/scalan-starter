package scalan.examples

import scalan.{ScalanCommunityDsl, ScalanCommunityDslSeq, ScalanCommunityDslExp}

/**
 * Created by Viktor Smirnov on 29.03.15.
 */
trait ExampleDsl extends ScalanCommunityDsl with MyArraysDsl

trait ExampleDslSeq extends ExampleDsl with MyArraysDslSeq with ScalanCommunityDslSeq

trait ExampleDslExp extends ExampleDsl with MyArraysDslExp with ScalanCommunityDslExp

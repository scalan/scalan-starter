package scalan.examples

import scalan._
import scalan.common.Default
import scalan.common.OverloadHack.Overloaded1

trait MyArrays { self: ExampleDsl =>

  /**
   * Optional type synonim to hide boilerplate of using Rep
   */
  type MyArr[A] = Rep[MyArray[A]]

  /**
   * User defined type
   */
  trait MyArray[A] extends Def[MyArray[A]] {
    implicit def elem: Elem[A]
    def length: Rep[Int]
    def values: Rep[Collection[A]]
    def apply(i: Rep[Int]): Rep[A]
    def map[B: Elem](f: Rep[A] => Rep[B]): MyArr[B] = MyArray(values.map(f))
    def mapBy[B: Elem](f: Rep[A => B]): MyArr[B] = MyArray(values.mapBy(f))
    def zip[B: Elem](ys: MyArr[B]): MyArr[(A, B)] = PairMyArray((values zip ys.values).convertTo[Collection[(A, B)]])
  }

  /**
   * Declare this implicit to specify how to build Collection descriptor (Elem) generically using 
   * descriptor of its element type.
   * This is a generic function that builds descriptors from descriptors
   * @tparam A type of array element
   * @return default descriptor for a given element type
   */
  implicit def defaultMyArrayElement[A:Elem]: Elem[MyArray[A]] = element[A] match {
    case _: BaseElem[_] => element[BaseMyArray[A]].asElem[MyArray[A]]
    case pe: PairElem[a, b] =>
      implicit val ea = pe.eFst
      implicit val eb = pe.eSnd
      element[PairMyArray[a, b]].asElem[MyArray[A]]
    case viewE: ViewElem[_, _] => element[BaseMyArray[A]].asElem[MyArray[A]]
    case e => ???(s"Element is $e")
  }

  /**
   * Companion for MyArray type. Naming convention is used here.
   * Generated companion MyArray is inherited from this trait.
   */
  trait MyArrayCompanion extends TypeFamily1[MyArray] {
    /**
     * Constructs MyArray from Collection and can be invoked using companion like this
     * val my = MyArray(arr)
     * @param arr
     * @tparam A
     * @return
     */
    def apply[A: Elem](arr: Rep[Collection[A]]): MyArr[A] = fromArray(arr)

    /**
     * Construct MyArray in a generic way using type descriptor of its elements.
     * This funtions takes input array and puts it into different structures depending on element type.
     * @param arr input array
     * @tparam A type of array elements
     * @return
     */
    def fromArray[A: Elem](arr: Rep[Collection[A]]): MyArr[A] = {
      element[A] match {
        case baseE: BaseElem[a] =>
          BaseMyArray[a](arr.asRep[Collection[a]])
        case pairE: PairElem[a, b] =>
          implicit val ea = pairE.eFst
          implicit val eb = pairE.eSnd
          val ps = arr.asRep[Collection[(a, b)]]
          val as = fromArray(ps.map { _._1 })
          val bs = fromArray(ps.map { _._2 })
          as zip bs //PairMyArray[a,b](as, bs)
        case viewE: ViewElem[a, b] =>
          BaseMyArray[b](arr.asRep[Collection[b]])
        case e => ???(s"Element is $e")
      }
    }

    /**
     * Another example of MyArray constructor. Uses core Collection primitives.
     * @param len number of elements in the new MyArray
     * @param v   value to put into each element of MyArray
     * @tparam A
     * @return
     */
    def replicate[A: Elem](len: Rep[Int], v: Rep[A]): MyArr[A] = {
      element[A] match {
        case baseE: BaseElem[a] =>
          BaseMyArray[a](Collection.replicate(len, v.asRep[a]))
        case pairElem: PairElem[a ,b] => {
          implicit val ea = pairElem.eFst
          implicit val eb = pairElem.eSnd
          val ps = v.asRep[(a, b)]
          val as = replicate(len, ps._1)
          val bs = replicate(len, ps._2)
          as zip bs
        }
        case viewElem: ViewElem[a, b] =>
          BaseMyArray(Collection.replicate(len, v))
        case e => ???(s"Element is $e")
      }
    }
  }

  abstract class BaseMyArray[A](val values: Rep[Collection[A]])(implicit val eA: Elem[A])
    extends MyArray[A] {
    def elem = eA
    def length = values.length
    def apply(i: Rep[Int]) = values(i)
  }

  abstract class PairMyArray[A, B](val values: Rep[Collection[(A, B)]])(implicit val eA: Elem[A], val eB: Elem[B])
    extends MyArray[(A, B)] {
    lazy val elem = element[(A, B)]
    def arr = as.arr zip bs.arr
    def as: Rep[Collection[A]] = values.convertTo[PairCollection[A, B]].as
    def bs: Rep[Collection[B]] = values.convertTo[PairCollection[A, B]].bs
    def apply(i: Rep[Int]) = values(i)
    def length = as.length
  }

  trait BaseMyArrayCompanion extends ConcreteClass1[BaseMyArray] {
  }

  trait PairMyArrayCompanion extends ConcreteClass2[PairMyArray] with MyArrayCompanion {
    def apply[A, B](as: Rep[MyArray[A]], bs: Rep[MyArray[B]])(implicit ea: Elem[A], eb: Elem[B]): PairMyArray[A, B] = {
      PairMyArray((as.values zip bs.values).convertTo[Collection[(A, B)]])
    }
  }
}

trait MyArraysDsl extends impl.MyArraysAbs with MyArrays { self: ExampleDsl => }

trait MyArraysDslSeq extends impl.MyArraysSeq with MyArraysDsl{ self: ExampleDslSeq => }

trait MyArraysDslExp extends impl.MyArraysExp with MyArraysDsl { self: ExampleDslExp => }

package scalan.examples

import scalan._
import scalan.common.Default
import scalan.common.OverloadHack.Overloaded1
import scala.reflect.runtime.universe._

package impl {
// Abs -----------------------------------
trait MyArraysAbs extends scalan.ScalanDsl with MyArrays {
  self: ExampleDsl =>

  // single proxy for each type family
  implicit def proxyMyArray[A](p: Rep[MyArray[A]]): MyArray[A] = {
    proxyOps[MyArray[A]](p)(scala.reflect.classTag[MyArray[A]])
  }

  // familyElem
  class MyArrayElem[A, To <: MyArray[A]](implicit _elem: Elem[A])
    extends EntityElem[To] {
    def elem = _elem
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(elem))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagA = elem.tag
      weakTypeTag[MyArray[A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[MyArray[A]] => convertMyArray(x) }
      tryConvert(element[MyArray[A]], this, x, conv)
    }

    def convertMyArray(x: Rep[MyArray[A]]): Rep[To] = {
      x.selfType1 match {
        case _: MyArrayElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have MyArrayElem[_, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def myArrayElement[A](implicit elem: Elem[A]): Elem[MyArray[A]] =
    cachedElem[MyArrayElem[A, MyArray[A]]](elem)

  implicit case object MyArrayCompanionElem extends CompanionElem[MyArrayCompanionAbs] {
    lazy val tag = weakTypeTag[MyArrayCompanionAbs]
    protected def getDefaultRep = MyArray
  }

  abstract class MyArrayCompanionAbs extends CompanionDef[MyArrayCompanionAbs] with MyArrayCompanion {
    def selfType = MyArrayCompanionElem
    override def toString = "MyArray"
  }
  def MyArray: Rep[MyArrayCompanionAbs]
  implicit def proxyMyArrayCompanionAbs(p: Rep[MyArrayCompanionAbs]): MyArrayCompanionAbs =
    proxyOps[MyArrayCompanionAbs](p)

  abstract class AbsBaseMyArray[A]
      (values: Rep[Collection[A]])(implicit eA: Elem[A])
    extends BaseMyArray[A](values) with Def[BaseMyArray[A]] {
    lazy val selfType = element[BaseMyArray[A]]
  }
  // elem for concrete class
  class BaseMyArrayElem[A](val iso: Iso[BaseMyArrayData[A], BaseMyArray[A]])(implicit val eA: Elem[A])
    extends MyArrayElem[A, BaseMyArray[A]]
    with ConcreteElem[BaseMyArrayData[A], BaseMyArray[A]] {
    override lazy val parent: Option[Elem[_]] = Some(myArrayElement(element[A]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA))
    }

    override def convertMyArray(x: Rep[MyArray[A]]) = BaseMyArray(x.values)
    override def getDefaultRep = BaseMyArray(element[Collection[A]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[BaseMyArray[A]]
    }
  }

  // state representation type
  type BaseMyArrayData[A] = Collection[A]

  // 3) Iso for concrete class
  class BaseMyArrayIso[A](implicit eA: Elem[A])
    extends EntityIso[BaseMyArrayData[A], BaseMyArray[A]] with Def[BaseMyArrayIso[A]] {
    override def from(p: Rep[BaseMyArray[A]]) =
      p.values
    override def to(p: Rep[Collection[A]]) = {
      val values = p
      BaseMyArray(values)
    }
    lazy val eFrom = element[Collection[A]]
    lazy val eTo = new BaseMyArrayElem[A](self)
    lazy val selfType = new BaseMyArrayIsoElem[A](eA)
    def productArity = 1
    def productElement(n: Int) = eA
  }
  case class BaseMyArrayIsoElem[A](eA: Elem[A]) extends Elem[BaseMyArrayIso[A]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new BaseMyArrayIso[A]()(eA))
    lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[BaseMyArrayIso[A]]
    }
  }
  // 4) constructor and deconstructor
  class BaseMyArrayCompanionAbs extends CompanionDef[BaseMyArrayCompanionAbs] with BaseMyArrayCompanion {
    def selfType = BaseMyArrayCompanionElem
    override def toString = "BaseMyArray"

    def apply[A](values: Rep[Collection[A]])(implicit eA: Elem[A]): Rep[BaseMyArray[A]] =
      mkBaseMyArray(values)
  }
  object BaseMyArrayMatcher {
    def unapply[A](p: Rep[MyArray[A]]) = unmkBaseMyArray(p)
  }
  lazy val BaseMyArray: Rep[BaseMyArrayCompanionAbs] = new BaseMyArrayCompanionAbs
  implicit def proxyBaseMyArrayCompanion(p: Rep[BaseMyArrayCompanionAbs]): BaseMyArrayCompanionAbs = {
    proxyOps[BaseMyArrayCompanionAbs](p)
  }

  implicit case object BaseMyArrayCompanionElem extends CompanionElem[BaseMyArrayCompanionAbs] {
    lazy val tag = weakTypeTag[BaseMyArrayCompanionAbs]
    protected def getDefaultRep = BaseMyArray
  }

  implicit def proxyBaseMyArray[A](p: Rep[BaseMyArray[A]]): BaseMyArray[A] =
    proxyOps[BaseMyArray[A]](p)

  implicit class ExtendedBaseMyArray[A](p: Rep[BaseMyArray[A]])(implicit eA: Elem[A]) {
    def toData: Rep[BaseMyArrayData[A]] = isoBaseMyArray(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoBaseMyArray[A](implicit eA: Elem[A]): Iso[BaseMyArrayData[A], BaseMyArray[A]] =
    reifyObject(new BaseMyArrayIso[A]()(eA))

  // 6) smart constructor and deconstructor
  def mkBaseMyArray[A](values: Rep[Collection[A]])(implicit eA: Elem[A]): Rep[BaseMyArray[A]]
  def unmkBaseMyArray[A](p: Rep[MyArray[A]]): Option[(Rep[Collection[A]])]

  abstract class AbsPairMyArray[A, B]
      (values: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B])
    extends PairMyArray[A, B](values) with Def[PairMyArray[A, B]] {
    lazy val selfType = element[PairMyArray[A, B]]
  }
  // elem for concrete class
  class PairMyArrayElem[A, B](val iso: Iso[PairMyArrayData[A, B], PairMyArray[A, B]])(implicit val eA: Elem[A], val eB: Elem[B])
    extends MyArrayElem[(A, B), PairMyArray[A, B]]
    with ConcreteElem[PairMyArrayData[A, B], PairMyArray[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(myArrayElement(pairElement(element[A],element[B])))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA), "B" -> Left(eB))
    }

    override def convertMyArray(x: Rep[MyArray[(A, B)]]) = PairMyArray(x.values)
    override def getDefaultRep = PairMyArray(element[Collection[(A, B)]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[PairMyArray[A, B]]
    }
  }

  // state representation type
  type PairMyArrayData[A, B] = Collection[(A, B)]

  // 3) Iso for concrete class
  class PairMyArrayIso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends EntityIso[PairMyArrayData[A, B], PairMyArray[A, B]] with Def[PairMyArrayIso[A, B]] {
    override def from(p: Rep[PairMyArray[A, B]]) =
      p.values
    override def to(p: Rep[Collection[(A, B)]]) = {
      val values = p
      PairMyArray(values)
    }
    lazy val eFrom = element[Collection[(A, B)]]
    lazy val eTo = new PairMyArrayElem[A, B](self)
    lazy val selfType = new PairMyArrayIsoElem[A, B](eA, eB)
    def productArity = 2
    def productElement(n: Int) = (eA, eB).productElement(n)
  }
  case class PairMyArrayIsoElem[A, B](eA: Elem[A], eB: Elem[B]) extends Elem[PairMyArrayIso[A, B]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new PairMyArrayIso[A, B]()(eA, eB))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[PairMyArrayIso[A, B]]
    }
  }
  // 4) constructor and deconstructor
  class PairMyArrayCompanionAbs extends CompanionDef[PairMyArrayCompanionAbs] with PairMyArrayCompanion {
    def selfType = PairMyArrayCompanionElem
    override def toString = "PairMyArray"

    def apply[A, B](values: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairMyArray[A, B]] =
      mkPairMyArray(values)
  }
  object PairMyArrayMatcher {
    def unapply[A, B](p: Rep[MyArray[(A, B)]]) = unmkPairMyArray(p)
  }
  lazy val PairMyArray: Rep[PairMyArrayCompanionAbs] = new PairMyArrayCompanionAbs
  implicit def proxyPairMyArrayCompanion(p: Rep[PairMyArrayCompanionAbs]): PairMyArrayCompanionAbs = {
    proxyOps[PairMyArrayCompanionAbs](p)
  }

  implicit case object PairMyArrayCompanionElem extends CompanionElem[PairMyArrayCompanionAbs] {
    lazy val tag = weakTypeTag[PairMyArrayCompanionAbs]
    protected def getDefaultRep = PairMyArray
  }

  implicit def proxyPairMyArray[A, B](p: Rep[PairMyArray[A, B]]): PairMyArray[A, B] =
    proxyOps[PairMyArray[A, B]](p)

  implicit class ExtendedPairMyArray[A, B](p: Rep[PairMyArray[A, B]])(implicit eA: Elem[A], eB: Elem[B]) {
    def toData: Rep[PairMyArrayData[A, B]] = isoPairMyArray(eA, eB).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoPairMyArray[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[PairMyArrayData[A, B], PairMyArray[A, B]] =
    reifyObject(new PairMyArrayIso[A, B]()(eA, eB))

  // 6) smart constructor and deconstructor
  def mkPairMyArray[A, B](values: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairMyArray[A, B]]
  def unmkPairMyArray[A, B](p: Rep[MyArray[(A, B)]]): Option[(Rep[Collection[(A, B)]])]

  registerModule(MyArrays_Module)
}

// Seq -----------------------------------
trait MyArraysSeq extends scalan.ScalanDslSeq with MyArraysDsl {
  self: ExampleDslSeq =>
  lazy val MyArray: Rep[MyArrayCompanionAbs] = new MyArrayCompanionAbs {
  }

  case class SeqBaseMyArray[A]
      (override val values: Rep[Collection[A]])(implicit eA: Elem[A])
    extends AbsBaseMyArray[A](values) {
  }

  def mkBaseMyArray[A]
    (values: Rep[Collection[A]])(implicit eA: Elem[A]): Rep[BaseMyArray[A]] =
    new SeqBaseMyArray[A](values)
  def unmkBaseMyArray[A](p: Rep[MyArray[A]]) = p match {
    case p: BaseMyArray[A] @unchecked =>
      Some((p.values))
    case _ => None
  }

  case class SeqPairMyArray[A, B]
      (override val values: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsPairMyArray[A, B](values) {
  }

  def mkPairMyArray[A, B]
    (values: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairMyArray[A, B]] =
    new SeqPairMyArray[A, B](values)
  def unmkPairMyArray[A, B](p: Rep[MyArray[(A, B)]]) = p match {
    case p: PairMyArray[A, B] @unchecked =>
      Some((p.values))
    case _ => None
  }
}

// Exp -----------------------------------
trait MyArraysExp extends scalan.ScalanDslExp with MyArraysDsl {
  self: ExampleDslExp =>
  lazy val MyArray: Rep[MyArrayCompanionAbs] = new MyArrayCompanionAbs {
  }

  case class ExpBaseMyArray[A]
      (override val values: Rep[Collection[A]])(implicit eA: Elem[A])
    extends AbsBaseMyArray[A](values)

  object BaseMyArrayMethods {
    object elem {
      def unapply(d: Def[_]): Option[Rep[BaseMyArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BaseMyArrayElem[_]] && method.getName == "elem" =>
          Some(receiver).asInstanceOf[Option[Rep[BaseMyArray[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[BaseMyArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[BaseMyArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BaseMyArrayElem[_]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[BaseMyArray[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[BaseMyArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[BaseMyArray[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[BaseMyArrayElem[_]] && method.getName == "apply" =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[BaseMyArray[A]], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BaseMyArray[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object BaseMyArrayCompanionMethods {
  }

  def mkBaseMyArray[A]
    (values: Rep[Collection[A]])(implicit eA: Elem[A]): Rep[BaseMyArray[A]] =
    new ExpBaseMyArray[A](values)
  def unmkBaseMyArray[A](p: Rep[MyArray[A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: BaseMyArrayElem[A] @unchecked =>
      Some((p.asRep[BaseMyArray[A]].values))
    case _ =>
      None
  }

  case class ExpPairMyArray[A, B]
      (override val values: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsPairMyArray[A, B](values)

  object PairMyArrayMethods {
    object arr {
      def unapply(d: Def[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairMyArrayElem[_, _]] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[PairMyArray[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object as {
      def unapply(d: Def[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairMyArrayElem[_, _]] && method.getName == "as" =>
          Some(receiver).asInstanceOf[Option[Rep[PairMyArray[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object bs {
      def unapply(d: Def[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairMyArrayElem[_, _]] && method.getName == "bs" =>
          Some(receiver).asInstanceOf[Option[Rep[PairMyArray[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[PairMyArray[A, B]], Rep[Int]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[PairMyArrayElem[_, _]] && method.getName == "apply" =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[PairMyArray[A, B]], Rep[Int]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairMyArray[A, B]], Rep[Int]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairMyArrayElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[PairMyArray[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object PairMyArrayCompanionMethods {
    // WARNING: Cannot generate matcher for method `apply`: Method's return type PairMyArray[A, B] is not a Rep
  }

  def mkPairMyArray[A, B]
    (values: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairMyArray[A, B]] =
    new ExpPairMyArray[A, B](values)
  def unmkPairMyArray[A, B](p: Rep[MyArray[(A, B)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: PairMyArrayElem[A, B] @unchecked =>
      Some((p.asRep[PairMyArray[A, B]].values))
    case _ =>
      None
  }

  object MyArrayMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[MyArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MyArrayElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[MyArray[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MyArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object values {
      def unapply(d: Def[_]): Option[Rep[MyArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MyArrayElem[_, _]] && method.getName == "values" =>
          Some(receiver).asInstanceOf[Option[Rep[MyArray[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MyArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[MyArray[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[MyArrayElem[_, _]] && method.getName == "apply" =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[MyArray[A]], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MyArray[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `map`: Method has function arguments f

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[MyArray[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[MyArrayElem[_, _]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[MyArray[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MyArray[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zip {
      def unapply(d: Def[_]): Option[(Rep[MyArray[A]], MyArr[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[MyArrayElem[_, _]] && method.getName == "zip" =>
          Some((receiver, ys)).asInstanceOf[Option[(Rep[MyArray[A]], MyArr[B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MyArray[A]], MyArr[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object MyArrayCompanionMethods {
    object apply {
      def unapply(d: Def[_]): Option[Rep[Collection[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem == MyArrayCompanionElem && method.getName == "apply" =>
          Some(arr).asInstanceOf[Option[Rep[Collection[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Collection[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromArray {
      def unapply(d: Def[_]): Option[Rep[Collection[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem == MyArrayCompanionElem && method.getName == "fromArray" =>
          Some(arr).asInstanceOf[Option[Rep[Collection[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Collection[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object replicate {
      def unapply(d: Def[_]): Option[(Rep[Int], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(len, v, _*), _) if receiver.elem == MyArrayCompanionElem && method.getName == "replicate" =>
          Some((len, v)).asInstanceOf[Option[(Rep[Int], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Int], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object MyArrays_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWTYgcRRR+PbOz87fsrppIFEfXZXQx6Ew2CDnsYZmdzEZlsrts7yGMIVLTUzPpWP2zXTVLt4eQUxC9iVfBgBchF/EkgngRxIMnEcGzpyQiORg8KKmq/pmen+5shPSh6Op69d6r7/veq759D3LUgVephggyawZmqKbK9wZlVbVlMp15F63ekODzuH/K+v7z9S+f/yYDSx2Yv4roeUo6UPRfWq4dvav4sA1FZGqYMsuhDF5uywh1zSIEa0y3zLpuGEOGugTX2zplG22Y61o97xCug9KGZc0yNQczrDYJohTT4HsBi4z0aF6Uc2/XHsUw6+IU9dgpDhykM54+j7Hs2+9jW/VMy/QMBotBaru2SIvb5HXDthwWhshzd1etXjidMxH/AE+3r6EjVOchBnWVObo54DvLNtLeRwO8w02E+RxPmGLSP/BsOc+2oUTxIQfobcMm8otrAwBn4KxMojbCpxbhUxP4VFXs6IjoHyCxuOdYrgf+o2QBXJu7eP0RLkIPuGX2qh9d1t59oJaNjNjsilTy8oTz3NFLCWqQVHAcf9z/hN6/cOtcBkodKOm00aXMQRqLUx6gVUamaTGZcwQgcgacrdUktmSUBreZkERRswwbmdxTAOUC54noms6Esfi2ELCTAH2e2Tg0VVxbic67knBeqZsmImTvznNvvHK3dSkDmfEQRe5S5cJ3QqcM8he9huMgL/AuxiUGSkNCLIaiOxrzKdEjHNbu/Nn74QxczkToBcGORxh3kaO//Vr+5bXNDBQ6Ut7bBA06HEDaItjYdZqWyTpQsI6w46/kjxARbzMJzPdwHw0JC2CN45HleDBYSSxEGwuwNqTolRCAsq/bHcvE1e296t/qT5/eFrJ0YMFf8SvzP/3cv78v9plULIN5nuIQ0xDgLC/pcchLzagQjsXFiJGSH1a1DPzU6n39yq2PmcReccfrfrd7jfvfkPteSKEh7D9f3bx58q8v3ntG1k2hqzMD2dUzj1E1ocifYFXAOFaLzaAPSymtjy+WtxDFgdwT5C3Gk9GaHCqcvBOxnc34ASqxbbFgp5RQL9KIQQY3wizmhIYfyfB0tpWofCpJvElknt1vnyD3Nr/LQO4dyPV5VdA25LrW0OyFkPPLimGXbYXflHHIOcTIQUYEsXxWYHTeiYylYVkZP9NEX0lRm40PhjbBb377z5UPb7xlS+lONaqZgEXTrdQKmWIHxtgRw4uPW5cX/n9C8bTW5Hg6VcVnJ1S8h3RnWsXh8vIsmeYn487SeMzvk9Z4koOtNAfTmE6DUYnt2UwRborBdJiYz0mmsrxNpuo+ucJTKEmRRlm0wm1k6MRbnx14ZqBE1mfKxcfCHvd5XGgnQ98Y2QSGhRAiBktBU8Au4tdAcD2u8W6xmtAt1OA24Lhff/DZzumfv/5D/uWUxL3Cb2Qz+g8edbTJ7lFq+dH4f20sYy46cdXIbB8CzuBfFGcMAAA="
}
}


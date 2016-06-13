package scalan.demo

import java.io.File

import scalan.{JNIExtractorOpsExp, Scalan}
import scalan.compilation.{KernelStore, KernelType}
import scalan.examples.Helpers
import scalan.monads.{MonadsDsl, MonadsDslStd, MonadsDslExp}

trait Example4 extends Scalan with MonadsDsl with Helpers {
  def sum[F[_]:Cont]
         (F: Monad[F])(n: Rep[Int])
         (f: Rep[F[Int]] => Rep[Int]): Rep[Int] = {
    import F.toMonadic
    f {
      SArray
        .rangeFrom0(n)
        .map(F.unit(_))
        .foldLeft[F[Int]](F.unit(0)) {
          case Pair(facc, fi) =>
            for { acc <- facc; i <- fi }
            yield acc + i
        }
    }
  }

  lazy val sumWithId = fun {(n: Rep[Int]) =>
    sum(Monad[Id])(n)(r => r)
  }

  type Env = (Int,String)
  type Read[A] = Reader[Env, A]
  val M = Monad[Read]
  lazy val sumWithReader = fun {(in: Rep[(Env,Int)]) =>
    val Pair(env, n) = in
    sum(M)(n)(r => r.run(env))
  }
}

class Example4Std extends MonadsDslStd with Example4

class Example4Exp extends MonadsDslExp with JNIExtractorOpsExp with Example4 {
  var doInvoke = true
  override def invokeAll = doInvoke
  override def rewriteDef[T](d: Def[T]) = d match {
    //TODO this rule works only for this particular tests, but can be generalized
    case ArrayFold(
          Def(ArrayMap(_xs, Def(_h: Lambda[a,_]))),
          Def(f: Lambda[e,Int]@unchecked),
          Def(g: Lambda[_,_])) => {
      val xs = _xs.asRep[Array[a]]
      val h = _h.self.asRep[a => (e => Int)]
      implicit val eA = xs.elem.eItem
      implicit val eE = f.eA

      fun { (env: Rep[e]) => {
        val ys: Rep[Array[Int]] = xs.map(x => h(x)(env))
        ys.foldLeft(f.self(env)){ case Pair(acc, y) =>
          acc + y
        }
      }}
    }
    case _ => super.rewriteDef(d)
  }

}


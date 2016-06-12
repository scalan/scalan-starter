package scalan.demo

import java.io.File

import scalan.{JNIExtractorOpsExp, Scalan}
import scalan.compilation.{KernelStore, KernelType}
import scalan.examples.Helpers
import scalan.monads.{MonadsDsl, MonadsDslStd, MonadsDslExp}

trait Example4 extends Scalan with MonadsDsl with Helpers {
  def sum[F[_]:Cont](F: Monad[F])(n: Rep[Int])(f: Rep[F[Int]] => Rep[Int]): Rep[Int] = {
    import F.toMonadic
    f {
      val is = SArray.rangeFrom0(n).map(i => F.unit(i))

      is.foldLeft[F[Int]](F.unit(0)) { (in: Rep[(F[Int],F[Int])]) =>
        val Pair(facc,fi) = in
        for {
          acc <- facc
          i <- fi
        } yield {
          (acc + i)
        }
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
    Def(ArrayMap(xs, Def(h: Lambda[a,_]))),
    Def(f: Lambda[e,Int]@unchecked),
    Def(g: Lambda[_,_])) => {
      val source = xs.asRep[Array[a]]
      val h1 = h.self.asRep[a => (e => Int)]
      implicit val eA = source.elem.eItem
      implicit val eE = f.eA
      //implicit val eC = f.eB

      fun { (env: Rep[e]) => {
        val ys: Rep[Array[Int]] = source.map(x => h1(x)(env))
        val res = ys.foldLeft(f.self(env)){ (p: Rep[(Int, Int)]) =>
          val Pair(acc, y) = p
          acc + y
        }
        res
      }}
    }
    case _ => super.rewriteDef(d)
  }

}


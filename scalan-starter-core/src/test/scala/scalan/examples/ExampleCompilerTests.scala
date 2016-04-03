package scalan.examples

import scala.language.reflectiveCalls
import scalan._
import scalan.compilation.{DummyCompiler, StructsCompiler}

class ExampleCompilerTests extends BaseViewTests {

  class Ctx extends TestCompilerContext {
    override val compiler = new DummyCompiler(new ScalanCake)
      with StructsCompiler[ScalanCake]
    import compiler.scalan._

    def noTuples[A, B](f: Rep[A => B]): Boolean = {
      val g = new PGraph(f)
      !g.scheduleAll.exists(tp => tp.rhs match {
        case First(_) => true
        case Second(_) => true
        case Tup(_, _) => true
        case _ => false
      })
    }

    def testFlattening[T](e: Elem[T], expected: Elem[_]) = {
      val iso = getFlatteningIso(e)
      val eFrom = iso.eFrom
      assertResult(expected)(eFrom)
      iso
    }

  }

  test("staging") {
    val ctx = new Ctx {
    }
    import ctx.compiler.scalan._
    ctx.test("aamvm", aamvm)
  }

  test("structWrapper") {
    val ctx = new Ctx {

      import compiler.scalan._

      def testWrapper[A, B](functionName: String,
                            f: => Exp[A => B], expectTuples: Boolean = false): compiler.CompilerOutput[A, B] = {
        val out = super.test(functionName, f)
        val hasTuples = !noTuples(out.common.graph.roots(0).asRep[A => B])
        assert(expectTuples && hasTuples || (!expectTuples && !hasTuples))
        out
      }
    }
    import ctx.compiler.scalan._
    ctx.testWrapper("aamvm", structWrapper(aamvm))
  }
}

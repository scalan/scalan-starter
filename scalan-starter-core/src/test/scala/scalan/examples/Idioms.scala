package scalan.examples

import scalan.ScalanDslExp

object Idioms {

  object Snippet1 {
    import scalan._
    val ctx = new ScalanDslStd
    import ctx._
    val x: Rep[Int] = 10
    val y           = x + 1
  }

  object Snippet2 {
    import scalan._
    val ctx = new ScalanCake {
      override val currentPass = new DefaultPass("mypass", Pass.defaultPassConfig.copy(constantPropagation = false))
    }
    import ctx._

    val x: Rep[Int] = 10
    val y           = x + 1
    y.show()
  }

  object Snippet3 {
    import scalan._
    val ctx = new ScalanCake {
      override val currentPass = new DefaultPass("mypass", Pass.defaultPassConfig.copy(constantPropagation = false))
    }
    import ctx._

    val x: Rep[Int] = 10
    val inc         = (x: Rep[Int]) => x + 1
    val y           = inc(x)
    y.show()
  }

  object Snippet4 {
    // Reified Lambdas
    import scalan._
    val ctx = new ScalanCake
    import ctx._

    val x: Rep[Int] = 10
    val inc         = mkLambda({ (x: Rep[Int]) => x + 1 }, mayInline = false)
    val y           = inc(x)
    y.show
    val y2 = inc.apply(x)
    y2.show
  }

  object Snippet5 { // lambda unfolding and constant propagation
    import scalan._
    val ctx = new ScalanCake
    import ctx._

    val x: Rep[Int] = 10
    val inc         = fun { (x: Rep[Int]) => x + 1 }
    val y           = inc(x)
    y.show
  }

  object Snippet6 {  // graph mirroring
    import scalan._
    val ctx = new ScalanCake
    import ctx._
    val calc = fun { (in: Rep[(Int, (Int, Int))]) =>
      val Pair(a, Pair(b, c)) = in
      a * c + b * c
    }

    val calcClone = ProgramGraph.transform(calc, NoRewriting)
    showGraphs(calc, calcClone)
  }

  object Snippet7 {
    import scalan._
    val ctx = new ScalanCake
    import ctx._
    val calc = fun { (in: Rep[(Int, (Int, Int))]) =>
      val Pair(a, Pair(b, c)) = in
      a * c + b * c
    }

    def lemma = postulate { (a: Rep[Int], b: Rep[Int], c: Rep[Int]) =>
      a * c + b * c  <=> (a + b) * c
    }
    val rw = new RulesRewriter(List(patternRewriteRule(lemma)))

    val calcOpt = ProgramGraph.transform(calc, rw)
    showGraphs(calc, calcOpt)
  }

  object Snippet8 {
    import scalan._
    import scalan.collections._
    import scalan.linalgebra._
    val ctx = new ScalanCake with LADslExp {}
    import ctx._
    val vvm = fun { p: Rep[(Collection[Double], Collection[Double])] =>
      val Pair(items1, items2) = p
      val v1: Rep[Vector[Double]] = DenseVector(items1)
      val v2: Rep[Vector[Double]] = DenseVector(items2)
      v1.dot(v2)
    }
    showGraphs(vvm)
  }

  object Snippet9 {
    import java.lang.reflect.Method
    import scalan._
    import scalan.collections._
    import scalan.linalgebra._
    val ctx = new ScalanCake with LADslExp { override def isInvokeEnabled(d: Def[_], m: Method) = true }
    import ctx._
    val vvm = fun { p: Rep[(Collection[Double], Collection[Double])] =>
      val Pair(items1, items2) = p
      val v1: Rep[Vector[Double]] = DenseVector(items1)
      val v2: Rep[Vector[Double]] = DenseVector(items2)
      v1.dot(v2)
    }
    showGraphs(vvm)
  }

  object Snippet10 {
    import scalan._
    import scalan.collections._
    val ctx = new ScalanCake with CollectionsDslExp {}
    import ctx._
    def fromArray[T: Elem](arr: Rep[Array[T]]): Coll[T] = implicitly[Elem[T]] match {
      case e: PairElem[a, b] =>
        implicit val ea = e.eFst
        implicit val eb = e.eSnd
        val pairs = arr.asRep[Array[(a, b)]]
        val as = fromArray[a](pairs.map { _._1 })
        val bs = fromArray[b](pairs.map { _._2 })
        as zip bs
      case e => CollectionOverArray(arr)
    }
    val arr: Rep[Array[Int]] = Array(1, 2, 3)
    val pairs: Rep[Array[(Int,Int)]] = Array((1,1), (2,2), (3,3))
    showGraphs(fromArray(arr), fromArray(pairs))
  }

  object Snippet11 {
    import java.lang.reflect.Method
    import scalan._
    val ctx = new ScalanCake { override def isInvokeEnabled(d: Def[_], m: Method) = true }
    import ctx._
    val iso = getStructWrapperIso(element[((Int, (Long, Double)), (String, Boolean))])
    showGraphs(iso.fromFun, iso.toFun)
  }

  object Snippet12 {
    import java.lang.reflect.Method
    import scalan._
    val ctx = new ScalanCake { override def isInvokeEnabled(d: Def[_], m: Method) = true }
    import ctx._
    val iso = getStructWrapperIso(element[(Array[(Long, Double)], (String, Boolean))])
    showGraphs(iso.fromFun, iso.toFun)
  }

  object Snippet13 {
    import java.lang.reflect.Method
    import scalan._
    import scalan.common._
    val ctx = new ScalanCake with SegmentsDslExp {
      override def isInvokeEnabled(d: Def[_], m: Method) = true
    }
    import ctx._
    val iso = isoInterval
    showGraphs(iso.fromFun, iso.toFun)
  }

  object Snippet14 {  // iso compositions
    import java.lang.reflect.Method
    import scalan._
    import scalan.common._
    val ctx = new ScalanCake with SegmentsDslExp {
      override def isInvokeEnabled(d: Def[_], m: Method) = true
    }
    import ctx._
    val interval = isoInterval // method generated in boilerplate for each virtualized class
    val slice = isoSlice
    val pairs = pairIso(interval, slice)
    val flatten = getStructWrapperIso(pairs.eFrom)
    val iso = flatten >> pairs
    showGraphs(iso.fromFun, iso.toFun)
  }

  object Snippet15 {
    import scalan._
    import scalan.linalgebra._
    class Ctx extends ScalanCake with LADslExp { override def invokeAll = true }
    val ctx = new Ctx
    import ctx._
    val sparse2dense = fun { v: Rep[SparseVector[Double]] =>
      DenseVector(v.items)
    }
    showGraphs(sparse2dense)
  }

  object Snippet16 {
    import scalan._
    import scalan.linalgebra._
    class Ctx extends ScalanCake with LADslExp { override def invokeAll = false }
    val ctx = new Ctx
    import ctx._
    def sparseData2denseData = fun { data: Rep[SparseVectorData[Double]] =>
      val v = SparseVector(data)
      DenseVector(v.items).toData
    }
    showGraphs(sparseData2denseData)
  }

  object Snippet17 {
    import scalan._
    import scalan.linalgebra._
    class Ctx extends ScalanCake with LADslExp { override def invokeAll = true }
    val ctx = new Ctx
    import ctx._
    def sparseData2denseData = fun { data: Rep[SparseVectorData[Double]] =>
      val v = SparseVector(data)
      DenseVector(v.items).toData
    }
    showGraphs(sparseData2denseData)
    val w = structWrapper(sparseData2denseData)
    w.show
  }

  object Snippet18 {
    import scalan._
    import scalan.linalgebra._
    class Ctx extends ScalanCake with LADslExp { override def invokeAll = true }
    val ctx = new Ctx
    import ctx._
    val vector2dense = fun { v: Rep[Vector[Double]] =>
      DenseVector(v.items)
    }
    val sparseData2denseData = fun { data: Rep[SparseVectorData[Double]] =>
      val v = SparseVector(data)
      vector2dense(v).toData
    }
    val constData2denseData = fun { data: Rep[ConstVectorData[Double]] =>
      val v = ConstVector(data)
      vector2dense(v).toData
    }
    showGraphs(sparseData2denseData, constData2denseData)
  }

  object Snippet19 {
    import scalan._
    import scalan.linalgebra._
    class Ctx extends ScalanCake with LADslExp { override def invokeAll = true }
    val ctx = new Ctx
    import ctx._
    val Some(c) = hasConverter(element[Array[SparseVector[Double]]], element[Array[DenseVector[Double]]])
    val sparseData2denseData = fun { data: Rep[Array[SparseVectorData[Double]]] =>
      val vs = data.map(SparseVector(_))
      c(vs).map(_.toData)
    }
    showGraphs(sparseData2denseData)
  }

  object Snippet20 {
    import scalan._
    import scalan.linalgebra._
    var doInvoke = true
    class Ctx extends ScalanCake with LADslExp { override def invokeAll = doInvoke }
    val ctx = new Ctx
    import ctx._
    lazy val mvm = fun { p: Rep[(Matrix[Double], Vector[Double])] =>
      val Pair(m, v) = p
      DenseVector(m.rows.mapBy( fun{ r => r dot v }))
    }
    showGraphs(mvm)
    lazy val ddmvmC = fun { p: Rep[(Collection[Collection[Double]], Collection[Double])] =>
      val Pair(m, v) = p
      val width = m(0).length
      val matrix: Matr[Double] = CompoundMatrix(m.map { r: Coll[Double] => DenseVector(r) }, width)
      val vector: Vec[Double] = DenseVector(v)
      mvm(matrix, vector).items
    }
    showGraphs(ddmvmC)
    lazy val ddmvmA = fun { p: Rep[(Array[Array[Double]], Array[Double])] =>
      val Pair(m, v) = p
      val matrix = CollectionOverArray(m.map(r => CollectionOverArray(r)))
      val vector = CollectionOverArray(v)
      ddmvmC(matrix, vector).arr
    }
    showGraphs(ddmvmA)

    def ddmvmL = fun { p: Rep[(List[List[Double]], List[Double])] =>
      val Pair(m, v) = p
      val matrix = CollectionOverList(m.map(r => CollectionOverList(r)))
      val vector = CollectionOverList(v)
      ddmvmC(matrix, vector).arr
    }
    showGraphs(ddmvmL)
  }
}

package com.mentisware.graph

import com.mentisware.advdesign.Matrix

class AdjMatrixGraph(
    directed: Boolean,
    val vertices: Vector[Vertex],
    val adjMat: Matrix[Double]) extends Graph {
  require(vertices.length == adjMat.rows && adjMat.rows == adjMat.cols)

  // assume that if there is no edge between u and v, then mat(u)(v) is set to PositiveInfinity
  
  def isDirected = directed
  def nVertices = vertices.length
  
  def edges =
    for {
      i <- 0 until adjMat.rows
      j <- 0 until adjMat.cols
      if adjMat(i)(j) != Double.PositiveInfinity && (i != j || adjMat(i)(j) != 0)
    } yield Edge(vertices(i), vertices(j), adjMat.m(i)(j))

  def nEdges = edges.length
  def edge(src: Vertex, dst: Vertex) = {
      val w = adjMat(src)(dst)
      if (w == Double.PositiveInfinity) None else Some(Edge(src, dst, w))
  }
  def weight(src: Vertex, dst: Vertex) = adjMat(src)(dst)
      
  def adjVertices(s: Vertex) =
    for {
      j <- 0 until adjMat.cols
      if adjMat.m(s)(j) != Double.PositiveInfinity
    } yield { (vertices(j), adjMat.m(s)(j)) }

  def transpose() =
    ((new AdjMatrixGraph(directed, vertices, adjMat.transpose())).asInstanceOf[this.type])

  def allPairsShortestPath_edgeDP: Option[Vector[PredGraph]] = { // all pairs shortest path by dynamic programming
    // The algorithm is
    // 1. Exploit the optimal substructure characteristic for k-edge paths.
    // 2. Exploit the fact that recursive path distance calculation has a similar characteristic to matrix multiplication.
    if (isDirected) {
      val n = nVertices
      
      // exploits from Matrix production implementation
      def reduce(i: Int, j: Int, a: Matrix[Double], b: Matrix[Double], pred: Vertex): (Double, Vertex) = {
        val ws = (a.m(i) zip b.m.map(_(j))).map(e => e._1 + e._2)
        val res = ((Double.PositiveInfinity, pred, 0) /: ws) { (res, w) =>
          val (l, p, k) = res
          if (l < w) (l, p, k+1)
          else if (k == j) (w, pred, k+1)      // when k == j, then input distance and pred is used.
          else (w, vertices(k), k+1)
        }
//        println("[reduce] for (i, j) = (" + i + "," + j + "), pred = " + pred + "new pred = " + res._2)
//        println("[reduce]      ws = " + ws)
        (res._1, res._2)
      }
  
      def extendShortestPath(
          L : Matrix[Double], W: Matrix[Double],
          PI: Vector[Vector[Vertex]]): (Matrix[Double], Vector[Vector[Vertex]]) = {
        assert(L.cols == n && W.rows == n && W.cols == n)
        
        val res = (0 until n).toVector.map { u => (0 until n).toVector.map( v => reduce(u, v, L, W, PI(u)(v))) }
        (Matrix(res.map(_.map(_._1))), res.map(_.map(_._2)))
      }
      
      // by using the similar idea of Bellman-Ford, (n-1) reductions will generate the shortest path.
      // if more reduction generate a different result, then there should be a negative cycle.
      val L, W = adjMat
      val PI0 = for (i <- 0 until n toVector) yield {
          for (j <- 0 until n toVector) yield {
            if (W(i)(j) == Double.PositiveInfinity) null else vertices(i)
          }
      }
      
      // in order to calculate PI, the fast algorithm cannot be applied because pred-update is not associative.
      val (ds, ps) = ((L, PI0) /: (2 to n-1)) { (x, _) => extendShortestPath(x._1, W, x._2) }
  
      // check if there is a negative weight cycle by one more reduction
      val (ds1, ps1) = extendShortestPath(ds, W, ps)
      if (ds != ds1) None
      else Some(for (i <- 0 until n toVector) yield new PredGraph(ps(i), null, ds.m(i)))
    } else None
  }
    
  private def getPreds(dists: Matrix[Double]): Vector[Vector[Vertex]] = {
    // calculate the predecessor, if shortest path p(i, j) = p(i, k) + e(k, j), then pred(i, j) = k
    // for such k, d(i, j) == d(i, k) + w(k, j)
    // corner case : k might be equal to i, but k should not be j
    val n = nVertices
    for (i <- 0 until n toVector) yield {
      for (j <- 0 until n toVector) yield {
        if (i == j) vertices(i)
        else {
          val d = dists(i)(j)
          if (d != Double.PositiveInfinity) {
            var k = 0
            while (k == j || d != dists(i)(k) + adjMat(k)(j)) k += 1    // this loop should terminate.
            assert(k >= 0 && k < n)
            vertices(k)
          } else null
        }
      }
    }
  }
  
  def allPairsShortestPath_fastEdgeDP: Option[Vector[PredGraph]] = { // all pairs shortest path by dynamic programming
    // The algorithm is almost same as above, with exception that
    // 1. the reductions log n times instead of n -1 by utilizing the associativity.
    // 2. the predecessors are calculated separately though "getPreds".
    if (isDirected) {
      val n = nVertices
      
      // exploits from Matrix production implementation
      def reduce(i: Int, j: Int, a: Matrix[Double], b: Matrix[Double]): Double = {
        val ws = (a.m(i) zip b.m.map(_(j))).map(e => e._1 + e._2)
        (Double.PositiveInfinity /: ws) { (res, w) =>
          if (res <= w) res else w
        }
      }
  
      def extendShortestPath(L : Matrix[Double], W: Matrix[Double]): Matrix[Double] = {
        assert(L.cols == n && W.rows == n && W.cols == n)
        
        val res = (0 until n).toVector.map { u => (0 until n).toVector.map( v => reduce(u, v, L, W)) }
        Matrix(res)
      }
      
      // by using the similar idea of Bellman-Ford, ceiling of (log (n-1)) reductions will generate the shortest path.
      // if more reduction generate a different result, then there should be a negative cycle.
      val L, W = adjMat
      var c = 2
      var dists: Matrix[Double] = L
      do {
        dists = extendShortestPath(dists, dists)
        c = c * c
      } while (c < n - 1)
  
      // check if there is a negative weight cycle by one more reduction
      val ds2 = extendShortestPath(dists, W)
      if (dists != ds2) None
      else {
        val ps = getPreds(dists)
//        println("dists = " + dists)
//        println("preds = " + ps)
        Some(for (i <- 0 until n toVector) yield new PredGraph(ps(i), null, dists.m(i)))
      }
    } else None
  }
  
  def allPairsShortestPath_floydwarshall: Option[Vector[PredGraph]] = {
    // The algorithm is a dynamic programming focusing on intermediate vertex instead of edge
    // it considers k in the range of (0, n-1)
    // 1. Initially, set D to W of the original graph
    // 2. By considering the possible intermediate vertex range of (0, k), update D
    
    if (isDirected) {
      val n = nVertices
      
      def updateDistance(
          D: Matrix[Double], P: Vector[Vector[Vertex]],
          k: Int): (Matrix[Double], Vector[Vector[Vertex]]) = {
        val res = for (i <- 0 until n toVector) yield {
          for (j <- 0 until n toVector) yield {
            val d = D(i)(k) + D(k)(j)
            if (D(i)(j) <= d) (D(i)(j), P(i)(j))
            else (d, P(k)(j))
          }
        }
        (Matrix(res.map(_.map(_._1))), res.map(_.map(_._2)))
      }
      
      val W = adjMat
      val PI0 = for (i <- 0 until n toVector) yield {
          for (j <- 0 until n toVector) yield {
            if (W.m(i)(j) == Double.PositiveInfinity) null else vertices(i)
          }
      }
      
      val (ds, ps) = ((W, PI0) /: (0 until n))((ds, k) => updateDistance(ds._1, ds._2, k))
//      println("ps1 = " + ps + "\nps2 = " + getPreds(ds))
      
      Some(for (i <- 0 until n toVector) yield new PredGraph(ps(i), null, ds.m(i)))
    } else None
  }
  
  def transitiveClosure: this.type = {
    // The algorithm is
    // 1. map the orginal matrix into boolean (0, 1) matrix
    // 2. for updateDistance, t_new(i, j) = t(i, j) | (t(i, k) & t(k, j))
    //
    // this can be also applied to unweighted graphs treating them as weighted ones
    
    val n = nVertices
    
    def updateDistance(D: Vector[Vector[Boolean]], k: Int): Vector[Vector[Boolean]] = {
      for (i <- 0 until n toVector) yield {
        for (j <- 0 until n toVector) yield {
            D(i)(j) || (D(i)(k) && D(k)(j))
        }
      }
    }
    
    val b = adjMat.m.map(_.map(w => if (w == Double.PositiveInfinity) false else true))
    
    val t = (b /: (0 until n))((ds, k) => updateDistance(ds, k))
//      println("ps1 = " + ps + "\nps2 = " + getPreds(ds))
    
    (new AdjMatrixGraph(isDirected, vertices,
        Matrix(t.map(_.map(p => if (p) 1.0 else 0.0))))).asInstanceOf[this.type]
  }
}

object AdjMatrixGraph {  
  def apply(isDirected: Boolean, vs: Vector[Vertex], edges: (Vertex, Vertex, Double)*): AdjMatrixGraph = {
    val n = vs.length
    val m = Array.tabulate(n, n)((i, j) => if (i == j) 0.0 else Double.PositiveInfinity)
    edges foreach { e =>
      if (m(e._1)(e._2) > e._3) {    // current implementation does not support multiple edges for (i, j)
        m(e._1)(e._2) = e._3
        if (!isDirected) m(e._2)(e._1) = e._3
      }
    }

    new AdjMatrixGraph(isDirected, vs, Matrix(m.toVector.map(_.toVector)))
  }
  
  def apply(isDirected: Boolean, nVs: Int, edges: (Int, Int, Double)*): AdjMatrixGraph = {
    val vs = (0 until nVs).map(Vertex(_)).toVector
    val es = edges.map(e => (vs(e._1), vs(e._2), e._3))
    apply(isDirected, vs, es: _*)
  }
}
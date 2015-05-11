package com.mentisware.graph

import com.mentisware.test.UnitSpec

trait GraphTestSet {
  def graphs = List(
      (false, 8, List((0, 4), (0, 1), (1, 5), (2, 3), (2, 5), (2, 6), (3, 6), (3, 7), (5, 6), (6, 7)).map(e => (e._1, e._2, 1.0))),
      (true, 8, List((0, 4), (1, 0), (4, 1), (1, 5), (5, 4), (2, 1), (2, 5), (6, 2), (6, 5), (3, 6), (7, 6), (3, 7), (7, 3)).map(e => (e._1, e._2, 1.0))),
      (true, 8, List((0, 1), (1, 4), (4, 0), (1, 5), (4, 5), (1, 2), (2, 3), (3, 2), (2, 6), (5, 6), (6, 5), (6, 7), (7, 7), (3, 7)).map(e => (e._1, e._2, 1.0))),
      (false, 9, List((0, 1, 4), (0, 7, 8), (1, 7, 11), (1, 2, 8), (2, 8, 2), (7, 8, 7), (6, 7, 1), (6, 8, 6), (2, 3, 7), (2, 5, 4), (5, 6, 2), (3, 4, 9), (4, 5, 10), (3, 5, 14)).map(e => (e._1, e._2, e._3.toDouble)))
  )
}

trait GraphBehavior { this: UnitSpec =>
  def checkProperty(g: Graph)(nVertices: Int, nEdges: Int, isDirected: Boolean) {
    it should "return basic properties correctly" in {
      (g.isDirected) should equal (isDirected)
      (g.nVertices) should equal (nVertices)
      if (isDirected)
        (g.nEdges) should equal (nEdges)
      else
        (g.nEdges) should equal (2 * nEdges)
    }
  }
  
  def traverseInBFS(g: Graph)(s: g.V, d: g.V) {
    val (pGraph, stamp) = g.bfsGraph(s)
    val p = pGraph.path(s, d)
    
    it should "traverse a graph in BFS" in {
      (pGraph.pred(s)) should equal (null)
      g.vertices foreach { v =>
        println(v + " (" + stamp.discover(v) + ", " + stamp.finish(v) + ")" )
      }
    }
    
    it should "return a BFS path correctly" in {
      def checkParent(path: List[g.V], p: g.V) {
        if (path != Nil) {
          assert(pGraph.pred(path.head) == p)
          checkParent(path.tail, path.head)
        }
      }
      
      if (!p.isEmpty)
        checkParent(p.tail, s)
      else
        assert(!(stamp.discover(s) < stamp.discover(d) && stamp.finish(s) > stamp.finish(d) ||
            stamp.discover(s) > stamp.discover(d) && stamp.finish(s) < stamp.finish(d)))
            
      println("path from " + s + " to " + d + " = " + p)
    }
  }

  def traverseInDFS(g: Graph)(s: g.V, d: g.V) {
    val (pGraph, stamp) = g.dfsGraph(s)
    val p = pGraph.path(s, d)
    
    it should "traverse a graph in DFS" in {
      (pGraph.pred(s)) should equal (null)
      g.vertices foreach { v =>
        println(v + " (" + stamp.discover(v) + ", " + stamp.finish(v) + ")" )
      }
    }
    
    it should "return a DFS path correctly" in {
      def checkParent(path: List[g.V], p: g.V) {
        if (path != Nil) {
          assert(pGraph.pred(path.head) == p)
          checkParent(path.tail, path.head)
        }
      }
      
      if (!p.isEmpty) checkParent(p.tail, s)
      println("path from " + s + " to " + d + " = " + p)
    }
  }
  
  def performTopologicalSort(g: Graph) {
    it should "sort a graph topologically" in {
      if (g.isDirected) {
        val vs = g.sortTopologically()
        (vs.length) should equal (g.nVertices)
        
        val (_, stamp) = g.dfsGraph()
        for (i <- (1 until vs.length)) {
          assert(stamp.finish(vs(i)) < stamp.finish(vs(i-1)))
        }
        
        println("topological sort : " + vs)
      } else {
          intercept[Exception] { g.sortTopologically() }
      }
    }
  }

  def calculateSCC(g: Graph) {
    it should "calculate strongly connected components" in {
      val scc = g.stronglyConnectedComponents
      
      println("strongly connected components : " + scc)
    }
  }

  def calculateMST(g: Graph) {
    if (!g.isDirected) {
      val res1 = g.MST_kruskal
      val res2 = g.MST_prim
      
      (res1._1.length) should equal (g.nVertices - 1)
      (res2._1.length) should equal (g.nVertices - 1)
      (res1._2) should equal (res2._2)
    }
  }
}
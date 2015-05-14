package com.mentisware.graph

import com.mentisware.test.UnitSpec

trait GraphTestSet {
  def graphs = List(
      (false, 8, List((0, 4), (0, 1), (1, 5), (2, 3), (2, 5), (2, 6), (3, 6), (3, 7), (5, 6), (6, 7)).map(e => (e._1, e._2, 1.0))),
      (true, 8, List((0, 4), (1, 0), (4, 1), (1, 5), (5, 4), (2, 1), (2, 5), (6, 2), (6, 5), (3, 6), (7, 6), (3, 7), (7, 3)).map(e => (e._1, e._2, 1.0))),
      (true, 8, List((0, 1), (1, 4), (4, 0), (1, 5), (4, 5), (1, 2), (2, 3), (3, 2), (2, 6), (5, 6), (6, 5), (6, 7), (7, 7), (3, 7)).map(e => (e._1, e._2, 1.0))),
      (false, 9, List((0, 1, 4), (0, 7, 8), (1, 7, 11), (1, 2, 8), (2, 8, 2), (7, 8, 7), (6, 7, 1), (6, 8, 6), (2, 3, 7), (2, 5, 4), (5, 6, 2), (3, 4, 9), (4, 5, 10), (3, 5, 14)).map(e => (e._1, e._2, e._3.toDouble))),
      (true, 6, List((0, 1, 5), (0, 2, 3), (1, 2, 2), (1, 3, 6), (2, 3, 7), (2, 4, 4), (2, 5, 2), (3, 4, 1), (3, 5, 1), (4, 5, 2)).map(e => (e._1, e._2, e._3.toDouble))),
      (true, 3, List((0, 1, 1), (1, 2, -3), (2, 0, 4))map(e => (e._1, e._2, e._3.toDouble))),
      (true, 3, List((0, 1, 1), (1, 2, -3), (2, 0, 1))map(e => (e._1, e._2, e._3.toDouble)))
  )
}

trait GraphBehavior { this: UnitSpec =>
  def checkProperty(g: Graph)(nVertices: Int, nEdges: Int, isDirected: Boolean) {
    it should "return basic properties correctly" in {
      (g.isDirected) should equal (isDirected)
      (g.nVertices) should equal (nVertices)
      
      // currently implementation of adjacent matrix implementation does not support multiple edges between (i, j)
      if (!g.isInstanceOf[AdjMatrixGraph]) {
        if (isDirected)
          (g.nEdges) should equal (nEdges)
        else
          (g.nEdges) should equal (2 * nEdges)
      }
    }
  }
  
  def traverseInBFS(g: Graph)(s: Vertex, d: Vertex) {
    val pGraph = g.bfsGraph(s)
    val p = pGraph.path(s, d)
    
    it should "traverse a graph in BFS" in {
      (pGraph.pred(s)) should equal (null)
      g.vertices foreach { v =>
        println(v + " (" + pGraph.discover(v) + ", " + pGraph.finish(v) + ")" )
      }
    }
    
    it should "return a BFS path correctly" in {
      def checkParent(path: List[Vertex], p: Vertex) {
        if (path != Nil) {
          assert(pGraph.pred(path.head) == p)
          checkParent(path.tail, path.head)
        }
      }
      
      if (!p.isEmpty)
        checkParent(p.tail, s)
      else
        assert(!(pGraph.discover(s) < pGraph.discover(d) && pGraph.finish(s) > pGraph.finish(d) ||
            pGraph.discover(s) > pGraph.discover(d) && pGraph.finish(s) < pGraph.finish(d)))
            
//      println("path from " + s + " to " + d + " = " + p)
    }
  }

  def traverseInDFS(g: Graph)(s: Vertex, d: Vertex) {
    val (pGraph, _) = g.dfsGraph(s)
    val p = pGraph.path(s, d)
    
    it should "traverse a graph in DFS" in {
      (pGraph.pred(s)) should equal (null)
      g.vertices foreach { v =>
        println(v + " (" + pGraph.discover(v) + ", " + pGraph.finish(v) + ")" )
      }
    }
    
    it should "return a DFS path correctly" in {
      def checkParent(path: List[Vertex], p: Vertex) {
        if (path != Nil) {
          assert(pGraph.pred(path.head) == p)
          checkParent(path.tail, path.head)
        }
      }
      
      if (!p.isEmpty) checkParent(p.tail, s)
//      println("path from " + s + " to " + d + " = " + p)
    }
  }
  
  def performTopologicalSort(g: Graph) {
    it should "sort a graph topologically" in {
      if (g.isDirected) {
        val (vs, isAcyclic) = g.sortTopologically()
        (vs.length) should equal (g.nVertices)
        
        val (stamp, _) = g.dfsGraph()
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
    it should "return same result through Kruskal and Prim algorithms" in {
      if (!g.isDirected) {
        val res1 = g.MST_kruskal
        val res2 = g.MST_prim
        
        (res1._1.length) should equal (g.nVertices - 1)
        (res2._1.length) should equal (g.nVertices - 1)
        (res1._2) should equal (res2._2)
      }
    }
  }


  def calculateShortestPathFrom(g: Graph)(s: Vertex) {
    val res1 = g.shortestPathFrom_bellmanford(s)
    val res2 = g.shortestPathFrom_dag(s)
    val res3 = if (res1 == None) None else g.shortestPathFrom_dijkstra(s)
    
    if (g.isDirected) {
      val (predGraph, isAcyclic) = g.dfsGraph(s)
      if (isAcyclic) {
        it should "return the same results through Bellman-Ford, DAG, and Dijkstra [DAG]" in {
          (res1) should not equal (None)
          (res2) should not equal (None)
          (res3) should not equal (None)
          
          val pathGraph1 = res1.get
          val pathGraph2 = res2.get
          val pathGraph3 = res3.get
          g.vertices foreach { v =>
            (pathGraph1.dist(s, v)) should equal (pathGraph2.dist(s, v))
            (pathGraph1.dist(s, v)) should equal (pathGraph3.dist(s, v))
          }
        }
      } else {
        it should "return the same results through Bellman-Ford and Dijkstra in case of no negative cycle [Directed Graph]" in {
          if (res1 != None) {
            (res3) should not equal (None)
            val pathGraph1 = res1.get
            val pathGraph3 = res3.get
            g.vertices foreach { v =>
              (pathGraph1.dist(s, v)) should equal (pathGraph3.dist(s, v))            
            }
          }
          (res2) should equal (None)
        }
      }
    } else {
      it should "return None [Undirected Graph]" in {
        (res1) should equal (None)
        (res2) should equal (None)
        (res3) should equal (None)
      }
    }
  }

  def calculateAllPairsShortestPath(g: Graph) {
    val res1 = g.allPairsShortestPath_edgeDP
    val res2 = g.allPairsShortestPath_fastEdgeDP
    val res3 = if (res1 != None) g.allPairsShortestPath_floydwarshall else None
          
    val n = g.nVertices
    
    if (g.isDirected) {
      it should "return the same results through edge-based DP algorithms [Directed Graph]" in {
        if (res1 == None) {
          (res2) should equal (None)
        } else {
          (res1) should not equal (None)
          (res2) should not equal (None)
          (res3) should not equal (None)
          
          val pathGraph1 = res1.get
          val pathGraph2 = res2.get
          val pathGraph3 = res3.get
          
          for (i <- 0 until n; s = g.vertices(i); j <- 0 until n; v = g.vertices(j)) {
            val pg = g.shortestPathFrom_bellmanford(s).get
            (pathGraph1(i).dist(s, v)) should equal (pathGraph2(i).dist(s, v))
            (pathGraph1(i).dist(s, v)) should equal (pathGraph3(i).dist(s, v))
            (pathGraph2(i).dist(s, v)) should equal (pg.dist(s, v))
            
            // check if the calculated path is correct
            val p1 = pathGraph1(i).path(s, v)
            val p2 = pathGraph2(i).path(s, v)
            val p3 = pathGraph3(i).path(s, v)
            val p4 = pg.path(s, v)
/*            println("p1: for source " + s + ": p = " + pathGraph1(i).p)
            println("p2: for source " + s + ": p = " + pathGraph2(i).p)
            println("p3: for source " + s + ": p = " + pathGraph3(i).p)
            println("p4: for source " + s + ": p = " + pg.p)

            
            println("p1 from " + s + " to " + v + ": " + p1)
            println("p2 from " + s + " to " + v + ": " + p2)
            println("p3 from " + s + " to " + v + ": " + p3)
            println("p4 from " + s + " to " + v + ": " + p4)
*/
            (pathGraph1(i).dist(s, v)) should equal (pathGraph1(i).calculateDist(p1))
            (pathGraph2(i).dist(s, v)) should equal (pathGraph2(i).calculateDist(p2))
            (pathGraph3(i).dist(s, v)) should equal (pathGraph3(i).calculateDist(p3))
          }
        }
      }
    } else {
      it should "return None through edge-based DP algorithms [Undirected Graph]" in {
        (res1) should equal (None)
        (res2) should equal (None)
        (res3) should equal (None)
      }
    }
  }

}
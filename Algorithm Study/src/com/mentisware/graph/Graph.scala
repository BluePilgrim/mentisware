package com.mentisware.graph
import com.mentisware.disjointset.DisjointSet
import com.mentisware.disjointset.ForestSet

class Vertex(val id: Int) {      // id start from 0
  override def toString = id.toString
}

object Vertex {
  def apply(id: Int) = new Vertex(id)
  implicit def v2id(v: Vertex): Int = v.id
}


// edge weight, for un-weighted graph, the valude is undefined or set to 1 for convenience.
case class Edge(src: Vertex, dst: Vertex, w: Double) {
  override def toString = "(" + src + ", " + dst +")(" + w + ")"
}

// vertex color
sealed abstract class VColor
object White extends VColor
object Gray extends VColor
object Black extends VColor



trait Graph {
  def isDirected: Boolean
  def vertices: Seq[Vertex]
  def nVertices: Int
  def edges: Seq[Edge]
  def nEdges: Int
  def edge(src: Vertex, dst: Vertex): Option[Edge]
  def weight(src: Vertex, dst: Vertex): Double
  def adjVertices(src: Vertex): Seq[(Vertex, Double)]
  
  
  def bfsGraph(ss: Vertex*): PredGraph = {
    // prepare traversal
    val parents = Array.fill[Vertex](nVertices)(null)
    val colors = Array.fill[VColor](nVertices)(White)
    val d = Array.fill(nVertices)(-1)
    val vQueue = new scala.collection.mutable.Queue[Vertex]
    var time = 0
    val dTime = Array.fill(nVertices)(0)
    val fTime = Array.fill(nVertices)(0)

    def visitInBFS(s: Vertex) {
      time += 1
      dTime(s) = time
      colors(s) = Gray
      d(s) = 0
      vQueue.enqueue(s)
      
      while (!vQueue.isEmpty) {
        val u = vQueue.dequeue()
        adjVertices(u) foreach { adj =>
          val v = adj._1
          if (colors(v) == White) {
            time += 1
            dTime(v) = time
            colors(v) = Gray
            d(v) = d(u) + 1
            parents(v) = vertices(u)
            vQueue.enqueue(v)
          }
        }
        time += 1
        fTime(u) = time
        colors(u) = Black
      }
    }
    
    ss foreach { s => if (colors(s) == White) visitInBFS(s) }
    
    vertices foreach { v =>
      if (colors(v) == White) visitInBFS(v)
    }
      
    new PredGraph(parents.toVector, (dTime zip fTime).toVector)
  }
  
  def dfsGraph(ss: Vertex*): (PredGraph, Boolean) = {  // also checks if the graph is cyclic or acyclic
    val parents = Array.fill[Vertex](nVertices)(null)
    val colors = Array.fill[VColor](nVertices)(White)
    var time = 0
    val dTime = Array.fill(nVertices)(0)
    val fTime = Array.fill(nVertices)(0)
    var isAcyclic = true
    
    def visitInDFS(u: Vertex) {
      require(colors(u) == White)
      time += 1
      dTime(u) = time
      colors(u) = Gray
      
      adjVertices(u) foreach { adj =>
        val v = adj._1
        if (colors(v) == White) {
          parents(v) = u
          visitInDFS(v)
        } else if (colors(v) == Gray) isAcyclic = false
      }
      
      time += 1
      fTime(u) = time
      colors(u) = Black
    }
    
    ss foreach { s => if (colors(s) == White) visitInDFS(s) }
    
    vertices foreach { v =>
      if (colors(v) == White) visitInDFS(v)
    }
    
    (new PredGraph(parents.toVector, (dTime zip fTime).toVector), isAcyclic)
  }
  
  def sortTopologically(): (Vector[Vertex], Boolean) = {    // also returns flag for acyclic
    if (isDirected) {
      val (predGraph, isAcyclic) = dfsGraph()
      
      val n = nVertices
      val vs = Array.fill[Vertex](2*n)(null)
      
      // stamp.finish is in a range of (n+1, 2n) - NOOOO! in case a forest
      vertices foreach { v =>
        val f = predGraph.finish(v)
//        assert(f >= n + 1 && f <= 2 * n)
        vs(2*n - f) = v
      }
      
      (vs.filter(_ != null).toVector, isAcyclic)
    } else error("no topological sort on undirected graph")
  }
  
  def transpose(): this.type                              // edges are reversed

  def stronglyConnectedComponents: DisjointSet[Vertex] = {
    // The algorithm is
    // 1. do DFS to generate finish times for each vertex
    // 2. transpose the graph
    // 3. do DFS for the transpose in higher finish-time first order (can use the topological sort order)
    val (scgForest, _) =
      if (isDirected) this.transpose().dfsGraph(this.sortTopologically()._1: _*) else dfsGraph()
    scgForest.connectedSets
  }
  
  def MST_kruskal: (Seq[Edge], Double) = {                // minimum spanning tree by Kruskal's algorithm
    // The algorithm is
    // 1. sort edges in nondecreasing order by weight (smaller-weight edge first)
    // 2. if an edge connects two different sets, choose the edge and union the sets
    if (!isDirected) {
      import com.mentisware.sort.Sorter    
      implicit val wOrdering: Ordering[Edge] = Ordering.by(_.w)  // specify ordering by weight
      
      var mst: List[Edge] = Nil
      var sum = 0.0
      val djsets: DisjointSet[Vertex] = ForestSet(vertices)
      val sortedEdges = Sorter.quickSort[Edge](edges.toList)
      sortedEdges foreach { e =>
        if (djsets.set(e.src) != djsets.set(e.dst)) {
          mst = e :: mst
          sum += e.w
          djsets.union(e.src, e.dst)
        }
      }
      
      assert(mst.length == nVertices - 1)
      (mst, sum)
    } else error("no minimum spanning tree for directed graph")
  }
  
  def MST_prim: (Seq[Edge], Double) = {                    // minimum spanning tree by prim's algorithm
    // The algorithm is
    // put vertices into min heap ordered by the weight(distance) to MST set
    // start from any vertex and get the minimum weight vertex
    // update distances to adjacent vertices
    if (!isDirected) {
      import com.mentisware.mheap.Element
      import com.mentisware.mheap.MergeableHeap
      import com.mentisware.mheap.FibHeap
      
      class Dist(val v: Vertex, var key: Double) extends Element[Double] {
        def updateKey(k: Double) = {
          val org = key
          key = k
          org
        }  
        override def toString = "(" + key + ", " + v + ")"
      }
      
      var mst: List[Edge] = Nil
      var sum = 0.0
      val distances = vertices.map(new Dist(_, Double.PositiveInfinity))
      val visited = Array.fill(nVertices)(false)
      val es = Array.fill[Edge](nVertices)(null)
      
      distances(0).key = 0.0      // choose vertex 0 as the root for the MST set
      val distHeap = FibHeap(distances)
      
      while (!distHeap.isEmpty) {
        val u = (distHeap.extractHead().get).asInstanceOf[Dist]
//        println("extracting :" + u)
        if (es(u.v) != null) mst = es(u.v) :: mst
        sum += u.key
        visited(u.v.id) = true
        
        adjVertices(u.v) foreach { adj =>
          val (v, w) = adj
          val d = distances(v)
          if (!visited(v) && d.key > w) {
            distHeap.updateKey(d, w)
            es(v) = Edge(u.v, v, w)
          }
        }
      }
      
      assert(es(0) == null)
      (mst, sum)
    } else error("no minimum spanning tree for directed graph")
  }

  def shortestPathFrom_bellmanford(s: Vertex): Option[PredGraph] = {  // single source shortest path
                                                                      // in case of negative cycle, returns None
    // The algorithm is to perform relaxation (Vertex - 1) times.
    // If the relaxation does not saturate after (Vertex - 1) times, then the graph has a negative cycle.
    if (isDirected) {
      val pred = Array.fill[Vertex](nVertices)(null)
      val dist = Array.fill[Double](nVertices)(Double.PositiveInfinity)
      val es = edges
      dist(s.id) = 0.0
      
      for (i <- (1 until nVertices)) {
        es foreach { e =>
          e match {
            case Edge(u, v, w) =>
              if (dist(v) > dist(u) + w) {
                dist(v) = dist(u) + w
                pred(v) = u
              }
          }
        }
      }
      
      es foreach { e =>
        e match {
          case Edge(u, v, w) =>
            if (dist(v) > dist(u) + w) return None
        }
      }
      
      Some(new PredGraph(pred.toVector, null, dist.toVector))
    } else None
  }
  
  def shortestPathFrom_dag(s: Vertex): Option[PredGraph] = {    // shortest path for dag
    // The algorithm is
    // 1. topologically sort the vertices
    // 2. perform relaxation onto vertices in the sorted order
    if (isDirected) {
      val (sortedVertices, isAcyclic) = sortTopologically()
      if (isAcyclic) {
        val pred = Array.fill[Vertex](nVertices)(null)
        val dist = Array.fill[Double](nVertices)(Double.PositiveInfinity)
        dist(s.id) = 0
        
        sortedVertices foreach { u =>
          adjVertices(u) foreach { e =>
            val (v, w) = e
            if (dist(v) > dist(u) + w) {
              dist(v) = dist(u) + w
              pred(v) = u
            }
          }
        }
        
        Some(new PredGraph(pred.toVector, null, dist.toVector))
      } else None
    } else None
  }

  // assume that the graph contains no negative-weight cycle
  def shortestPathFrom_dijkstra(s: Vertex): Option[PredGraph] = {
    // The algorithm is similar to Prim's algorithm for MST
    // put vertices into min heap ordered by the weight(distance) to source
    // start from any vertex and get the minimum distance vertex
    // update distances to the source
    if (isDirected) {
      import com.mentisware.mheap.Element
      import com.mentisware.mheap.MergeableHeap
      import com.mentisware.mheap.FibHeap
      
      class Dist(val v: Vertex, var key: Double) extends Element[Double] {
        def updateKey(k: Double) = {
          val org = key
          key = k
          org
        }  
        override def toString = "(" + key + ", " + v + ")"
      }

      val distances = vertices.toVector.map(new Dist(_, Double.PositiveInfinity))
      val pred = Array.fill[Vertex](nVertices)(null)
      
      distances(s).key = 0.0      // choose vertex 0 as the root for the MST set
      val distHeap = FibHeap(distances)
      
      while (!distHeap.isEmpty) {
        val u = (distHeap.extractHead().get).asInstanceOf[Dist]
//        println("extracting :" + u)
        val uD = u.key
        adjVertices(u.v) foreach { adj =>
          val (v, w) = adj
          val d = distances(v)
          val vD = d.key
          if (vD > uD + w) {
            distHeap.updateKey(d, uD + w)
            pred(v) = u.v
          }
        }
      }
      
      assert(pred(s) == null)
      Some(new PredGraph(pred.toVector, null, distances.map(_.key)))
    } else None
  }

  // currently, implemented for adjacent matrix representation
  def allPairsShortestPath_edgeDP: Option[Vector[PredGraph]]
  def allPairsShortestPath_fastEdgeDP: Option[Vector[PredGraph]]
  def allPairsShortestPath_floydwarshall: Option[Vector[PredGraph]]
  def transitiveClosure: this.type
  
  def addDummySource(): (this.type, Vertex)  // add a dummy source connecting to all vertices with 0 weight
  def reweight(f: (Vertex, Vertex) => Double) : this.type
  def allPairsShortestPath_johnson: Option[Vector[PredGraph]] = {  // good for sparse graph
    // generate a new graph which introduces a new dummy source, s
    val (g, s) = addDummySource()
    
    // calculate a new weight function from dist(s, v)
    g.shortestPathFrom_bellmanford(s) match {
      case None => None
      case Some(pg) =>
        val reweightedG = reweight((u, v) => weight(u, v) + pg.dist(s, u) - pg.dist(s, v))
        val ps = reweightedG.vertices.map(reweightedG.shortestPathFrom_dijkstra(_).get).toVector
        val ds = for (i <- 0 until nVertices toVector) yield {
          for (j <- 0 until nVertices toVector) yield {
            ps(i).d(j) + pg.dist(s, vertices(j)) - pg.dist(s, vertices(i))
          }
        }
        
//        println("for dummy source: " + g.edges)
//        println("distance from s: " + pg.d)
//        println("reweighted edges: " + reweightedG.edges)
//        println("restored distance: " + ds)
        Some((ps zip ds).map(x => new PredGraph(x._1.p, x._1.stamp, x._2)))
    }
  }
  
  def error(m: String) = throw new NoSuchElementException(m)

  class PredGraph(
      val p: Vector[Vertex],
      val stamp: Vector[(Int, Int)] = null,
      val d: Vector[Double] = null) {
    def pred(v: Vertex) = p(v)
    
    def path(src: Vertex, dst: Vertex): List[Vertex] = {
      def reversePath(s: Vertex, d: Vertex): List[Vertex] =
        if (s == d) List(s)
        else if (pred(d) == null || pred(d) == d) Nil
        else d :: reversePath(s, pred(d))
        
      val res = reversePath(src, dst).reverse
      if (res.isEmpty || res.head != src) Nil else res
    }
    
    def calculateDist(p: List[Vertex]): Double = {
      if (p == Nil) Double.PositiveInfinity
      else if (p.tail == Nil) 0.0
      else {
        val u = p.head
        val v = p.tail.head
        edge(u, v) match {
          case Some(e) => e.w + calculateDist(p.tail)
          case _ => Double.PositiveInfinity
        }
      }
    }

    def dist(src: Vertex, dst: Vertex) = {
      if (d != null) {
        require(d(src) == 0.0)
        d(dst)
      } else {
        val p = path(src, dst)
        if (p == Nil) Double.PositiveInfinity
        else calculateDist(p)
      }
    }
    
    def connectedSets: DisjointSet[Vertex] = {
      val sets = ForestSet(vertices)
      vertices foreach { v =>
        val u = pred(v)
        if (u != null && sets.set(u) != sets.set(v)) sets.union(u, v)
      }
      sets
    }
    
    // for time stamping
    def discover(v: Vertex) = stamp(v)._1
    def finish(v: Vertex) = stamp(v)._2
  }
}
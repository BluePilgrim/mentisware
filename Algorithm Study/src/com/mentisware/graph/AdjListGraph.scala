package com.mentisware.graph
import com.mentisware.disjointset.DisjointSet
import com.mentisware.disjointset.ForestSet

// assume that a vertex's id is set to the index of the adjacency list

case class AdjVertex(id: Int) extends Vertex {
  override def toString = id.toString
}

case class Adjacency(s: AdjVertex, adjVertices: List[(AdjVertex, Double)])

// vertex color
sealed abstract class VColor
object White extends VColor
object Gray extends VColor
object Black extends VColor


class AdjListGraph(val adjList: Vector[Adjacency], directed: Boolean) extends Graph {  
  type V = AdjVertex
  class Parents(val p: Vector[V]) extends PredGraph {
    def pred(v: V) = p(v.id)    
    def path(src: V, dst: V) = {
      def reversePath(s: V, d: V): List[V] =
        if (s == d) List(s)
        else if (pred(d) == null) Nil
        else d :: reversePath(s, pred(d))
        
      val res = reversePath(src, dst).reverse
      if (res.isEmpty || res.head != src) Nil else res
    }
    
    def connectedSets = {
      val sets = ForestSet(vertices)
      vertices foreach { v =>
        val u = pred(v)
        if (u != null && sets.set(u) != sets.set(v)) sets.union(u, v)
      }
      sets
    }
  }
  class TravTime(val ds: Vector[Int], val fs: Vector[Int]) extends TimeStamp {
    def discover(v: V) = ds(v.id)
    def finish(v: V) = fs(v.id)
  }
  
  def isDirected = directed
  def vertices = adjList.map(_.s)
  def nVertices = adjList.length
  
  def vertex(id: Int) = adjList(id).s
  
  def edges = adjList.flatMap(v => v.adjVertices.map(d => Edge(v.s, d._1, d._2)))
  def nEdges = edges.length
  
  def bfsGraph(ss: V*) = {
    // prepare traversal
    val nVtx = nVertices
    val parents = Array.fill[V](nVtx)(null)
    val colors = Array.fill[VColor](nVtx)(White)
    val d = Array.fill(nVtx)(-1)
    val vQueue = new scala.collection.mutable.Queue[Int]
    var time = 0
    val dTime = Array.fill(nVtx)(0)
    val fTime = Array.fill(nVtx)(0)

    def visitInBFS(id: Int) {
      time += 1
      dTime(id) = time
      colors(id) = Gray
      d(id) = 0
      vQueue.enqueue(id)
      
      while (!vQueue.isEmpty) {
        val u = vQueue.dequeue()
        adjList(u).adjVertices foreach { adj =>
          val v = adj._1.id
          if (colors(v) == White) {
            time += 1
            dTime(v) = time
            colors(v) = Gray
            d(v) = d(u) + 1
            parents(v) = adjList(u).s
            vQueue.enqueue(v)
          }
        }
        time += 1
        fTime(u) = time
        colors(u) = Black
      }
    }
    
    ss foreach { s => if (colors(s.id) == White) visitInBFS(s.id) }
    
    for (id <- (0 until nVtx))
      if (colors(id) == White) visitInBFS(id)
      
    (new Parents(parents.toVector), new TravTime(dTime.toVector, fTime.toVector))
  }
  
  def dfsGraph(ss: V*) = {
    val nVtx = nVertices
    val parents = Array.fill[V](nVtx)(null)
    val colors = Array.fill[VColor](nVtx)(White)
    var time = 0
    val dTime = Array.fill(nVtx)(0)
    val fTime = Array.fill(nVtx)(0)
    
    def visitInDFS(u: Int) {
      require(colors(u) == White)
      time += 1
      dTime(u) = time
      colors(u) = Gray
      
      adjList(u).adjVertices foreach { adj =>
        val v = adj._1.id
        if (colors(v) == White) {
          parents(v) = adjList(u).s
          visitInDFS(v)
        }
      }
      
      time += 1
      fTime(u) = time
      colors(u) = Black
    }
    
    ss foreach { s => if (colors(s.id) == White) visitInDFS(s.id) }
    
    for (id <- (0 until nVtx)) {
      if (colors(id) == White) visitInDFS(id)
    }
    
    (new Parents(parents.toVector), new TravTime(dTime.toVector, fTime.toVector))
  }
  
  def sortTopologically() = {
    if (isDirected) {
      val (_, stamp) = dfsGraph()
      val n = nVertices
      val vs = Array.fill[AdjVertex](2*n)(null)
      
      // stamp.finish is in a range of (n+1, 2n) - NOOOO! in case a forest
      adjList foreach { adj =>
        val v = adj.s
        val f = stamp.finish(v)
//        assert(f >= n + 1 && f <= 2 * n)
        vs(2*n - f) = v
      }
      
      vs.filter(_ != null).toVector
    } else error("no topological sort on undirected graph")
  }
  
  def transpose() = {
    if (isDirected) {
      val edges = adjList.flatMap { adj =>
        adj.adjVertices.map(x => (x._1, adj.s, x._2))
      }
      
      AdjListGraph(true, vertices.toVector, edges: _*).asInstanceOf[this.type]
    } else this
  }
  
  def stronglyConnectedComponents: DisjointSet[V] = {
    // The algorithm is
    // 1. do DFS to generate finish times for each vertex
    // 2. transpose the graph
    // 3. do DFS for the transpose in higher finish-time first order (can use the topological sort order)
    val (scgForest, _) =
      if (isDirected) this.transpose().dfsGraph(this.sortTopologically(): _*) else dfsGraph()
    scgForest.connectedSets
  }
  
  def MST_kruskal = {
    // The algorithm is
    // 1. sort edges in nondecreasing order by weight (smaller-weight edge first)
    // 2. if an edge connects two different sets, choose the edge and union the sets
    if (!isDirected) {
      import com.mentisware.sort.Sorter    
      implicit val wOrdering: Ordering[Edge] = Ordering.by(_.w)  // specify ordering by weight
      
      var mst: List[Edge] = Nil
      var sum = 0.0
      val djsets:DisjointSet[Vertex] = ForestSet(vertices)
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
  
  def MST_prim = {
    // The algorithm is
    // put vertices into min heap ordered by the weight(distance) to MST set
    // start from any vertex and get the minimum weight vertex
    // update distances to adjacent vertices
    if (!isDirected) {
      import com.mentisware.mheap.Element
      import com.mentisware.mheap.MergeableHeap
      import com.mentisware.mheap.FibHeap
      
      class Dist(val v: AdjVertex, var key: Double) extends Element[Double] {
        def updateKey(k: Double) = {
          val org = key
          key = k
          org
        }  
        override def toString = "(" + key + ", " + v + ")"
      }
      
      var mst: List[Edge] = Nil
      var sum = 0.0
      val distances = adjList.map(adj => new Dist(adj.s, Double.PositiveInfinity))
      val visited = Array.fill(nVertices)(false)
      val es = Array.fill[Edge](nVertices)(null)
      
      distances(0).key = 0.0      // choose vertex 0 as the root for the MST set
      val distHeap = FibHeap(distances)
      
      while (!distHeap.isEmpty) {
        val u = (distHeap.extractHead match { case Some(x) => x }).asInstanceOf[Dist]
        println("extracting :" + u)
        if (es(u.v.id) != null) mst = es(u.v.id) :: mst
        sum += u.key
        visited(u.v.id) = true
        
        adjList(u.v.id).adjVertices foreach { adj =>
          val (v, w) = adj
          val d = distances(v.id)
          if (!visited(v.id) && d.key > w) {
            distHeap.updateKey(d, w)
            es(v.id) = Edge(u.v, v, w)
          }
        }
      }
      
      assert(es(0) == null)
      (mst, sum)
    } else error("no minimum spanning tree for directed graph")
  }
}

object AdjListGraph {
/*
  // automatic conversion of unweighted graph
  implicit def unweighted2weighted(x: (AdjVertex, AdjVertex)): (AdjVertex, AdjVertex, Double) =
    (x._1, x._2, 1.0)
*/
  
  def apply(isDirected: Boolean, vs: Vector[AdjVertex], edges: (AdjVertex, AdjVertex, Double)*): AdjListGraph = {
    val adjList = Array.fill[List[(AdjVertex, Double)]](vs.length)(Nil)
    edges foreach { e =>
      val s = e._1
      val d = e._2
      val w = e._3
      if (!isDirected) {
        adjList(d.id) = ((s, w) :: adjList(d.id))
      }
      adjList(s.id) = ((d, w) :: adjList(s.id))
    }
    
    new AdjListGraph((vs zip adjList).map(x => new Adjacency(x._1, x._2)), isDirected)
  }
  
  def apply(isDirected: Boolean, nVs: Int, edges: (Int, Int, Double)*): AdjListGraph = {
    val vs = (0 until nVs).map(AdjVertex).toVector
    val es = edges.map(e => (vs(e._1), vs(e._2), e._3))
    apply(isDirected, vs, es: _*)
  }
  
/*
  def apply(isDirected: Boolean)(vs: Vector[AdjVertex], unweightedPairs: (AdjVertex, AdjVertex)*) = {
    val adjList = Array.fill[List[(AdjVertex, Double)]](vs.length)(Nil)
    unweightedPairs foreach { e =>
      val s = e._1
      val d = e._2
      if (!isDirected) {
        adjList(d.id) = ((s, 1.0) :: adjList(d.id))
      }
      adjList(s.id) = ((d, 1.0) :: adjList(s.id))
    }
    
    new AdjListGraph((vs zip adjList).map(x => new Adjacency(x._1, x._2)), isDirected)
  }
*/
}
package com.mentisware.graph


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
  
  def bfsGraph(s: V*) = {
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
    
    val start = if (s.isEmpty) 0 else s.head.id
    visitInBFS(start)
    
    for (id <- (0 until nVtx))
      if (colors(id) == White) visitInBFS(id)
      
    (new Parents(parents.toVector), new TravTime(dTime.toVector, fTime.toVector))
  }
  
  def dfsGraph(s: V*) = {
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
    
    val start = if (s.isEmpty) 0 else s.head.id
    visitInDFS(start)
    
    for (id <- (0 until nVtx)) {
      if (colors(id) == White) visitInDFS(id)
    }
    
    (new Parents(parents.toVector), new TravTime(dTime.toVector, fTime.toVector))
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
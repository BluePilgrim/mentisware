package com.mentisware.graph

abstract class Vertex

// edge weight, for un-weighted graph, the valude is undefined or set to 1 for convenience.
case class Edge(src: Vertex, dst: Vertex, w: Double)


trait Graph {
  type V <: Vertex
  trait PredGraph {
    def pred(v: V): V
    def path(src: V, dst: V): List[V]
  }
  trait TimeStamp {
    def discover(v: V): Int
    def finish(v: V): Int
  }
  
  def isDirected: Boolean
  def vertices: Seq[V]
  def nVertices: Int
  def edges: Seq[Edge]
  def nEdges: Int
  
  def bfsGraph(s: V*): (PredGraph, TimeStamp)            // only consider the first vertex
  def dfsGraph(s: V*): (PredGraph, TimeStamp)
  
  def error(m: String) = throw new NoSuchElementException(m)
}
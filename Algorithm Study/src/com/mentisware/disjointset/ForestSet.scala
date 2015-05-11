package com.mentisware.disjointset

// a forest of reversed tree is implemented as a disjoint set.
// union-by-rank and path compression heuristic are used.

class ReverseTree[T](val data: T, var parent: ReverseTree[T] = null, var rank: Int = 0) extends Set[T] {
  def root: ReverseTree[T] =
    if (parent == null) this
    else {
      parent = parent.root    // path compression
      parent
    }
  
  def representative = root.data
  def add(x: T) =
    if (parent == null) {
      if (rank == 0) rank = 1
      new ReverseTree(x, this, 0)
    } else new ReverseTree(x, parent, 0)
}

class ForestSet[T](var sets: List[ReverseTree[T]],
    private val elemTable: scala.collection.mutable.Map[T, ReverseTree[T]]) extends DisjointSet[T] {
  def set(x: T) = elemTable(x).root

  def addSet(x: T) = {
    // cannot deal with duplicates
    val s = new ReverseTree(x, null, 0)
    elemTable += (x -> s)
    sets = (s :: sets)
    this
  }
  
  def union(x: T, y: T) = {
    val s1 = set(x)
    val s2 = set(y)
    
    if (s1 != s2) {
      // union-by-rank
      if (s1.rank > s2.rank) {
        s2.parent = s1
        sets = sets.filter(_ != s2)
        elemTable += (y -> s1)
      }
      else {
        s1.parent = s2
        sets = sets.filter(_ != s1)
        elemTable += (x -> s2)
        if (s1.rank == s2.rank) s2.rank += 1
      }
    }
    
    this
  }
  
  override def toString = {
//    elemTable.map(x => (x._1, x._2.representative)).mkString(", ")
    elemTable.mapValues(_.representative).groupBy(_._2).mapValues(_.map(_._1)).values.mkString(", ")
  }
}

object ForestSet {
  def apply[T](initialData: Seq[T]): ForestSet[T] = {
    val data = initialData.toList
    val sets = data.map(new ReverseTree(_, null, 0))
    val elemTable = scala.collection.mutable.Map[T, ReverseTree[T]]((data zip sets): _*)
    
    new ForestSet(sets, elemTable)
  }
}
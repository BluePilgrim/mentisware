package com.mentisware.disjointset

// the set is pseudo-persistent with almost no variables
// -> pseudo-persistent does not make sense. for efficiency, I determined to use variables.
// persistent data structure requires nullifies the weighted set heuristic.
// element list of a set is maintained via a list, which can be used for union.
// Membership can be checked with a hash map.

class LinkedSet[T](var elems: List[T], var size: Int) extends Set[T] {
  def representative = elems.head
  def add(x: T) = { elems = x :: elems; size += 1; this }
//  def isMember(x: T) = djset.set(x) == this
  def mergeWith(that: LinkedSet[T]) = {
    elems = that.elems ::: this.elems
    size += that.size
    this
  }
}

class LinkedDisjointSet[T](var sets: List[LinkedSet[T]],
    private val elemTable: scala.collection.mutable.Map[T, LinkedSet[T]]) extends DisjointSet[T] {
//  val sets = data.map(l => LinkedSet(l._1, l._2))
//  val elemTable = sets.flatMap(s => s.elems.map((_ -> s))).toMap

  def set(x: T) = elemTable(x)

  def addSet(x: T) = {
    val s = new LinkedSet(List(x), 1)
    val dup = elemTable.getOrElse(x, null)
    if (dup != null) {
      // remove a duplicate
      val es = dup.elems.filter(_ != x)
      if (es == Nil) sets = sets.filter(_ != dup)
      else {  
        dup.elems = es
        dup.size -= 1
      }
      elemTable.remove(x)
    }
    elemTable += (x -> s)
    sets = (s :: sets)
    this
  }
  
  def union(x: T, y: T) = {
    def mergeAndUpdate(s1: LinkedSet[T], s2: LinkedSet[T]) = {
      val xs = s2.elems.map((_, s1))
      elemTable ++= xs    // update the set memberships of elements in s2
      s1 mergeWith s2                // merge elements of s2 into s1
      sets = sets.filter(_ != s2)    // remove s2 from the set list   
      s1
    }
    
    val s1 = set(x)
    val s2 = set(y)
    
    // use the weighted-union heuristic
    if (s1 != s2)
      if (s1.size >= s2.size) mergeAndUpdate(s1, s2) else mergeAndUpdate(s2, s1)
    this
  }
}

object LinkedDisjointSet {
  def apply[T](initialData: Seq[T]): LinkedDisjointSet[T] = {
    val data = initialData.toList
    val sets = data.map(x => new LinkedSet(List(x), 1))
    val elemTable = scala.collection.mutable.Map[T, LinkedSet[T]]((data zip sets): _*)
    
    new LinkedDisjointSet(sets, elemTable)
  }
  
  def build[T](initialData: Seq[Seq[T]]): LinkedDisjointSet[T] = {
    val sets = initialData.map(x => new LinkedSet(x.toList, x.length)).toList
    val mapdata = sets.flatMap(s => s.elems.map(x => (x, s)))
    val elemTable = scala.collection.mutable.Map[T, LinkedSet[T]](mapdata: _*)
    
    new LinkedDisjointSet(sets, elemTable)
  }
}
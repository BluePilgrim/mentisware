package com.mentisware.disjointset

import com.mentisware.test.UnitSpec

trait LinkedDisjointSetTestSet {
  def set1 = 1 to 10
}

class LinkedDisjointSetTest extends UnitSpec with DisjointSetBehavior with LinkedDisjointSetTestSet {
  "Linked Disjoint Set" should behave like maintainSetsCorrectly(set1)(LinkedDisjointSet.apply[Int])
  it should "perform union with weight-union heuristic" in {
    val djset = LinkedDisjointSet.build(List(1 to 6, 7 to 10))
    val rep1 = djset.set(1)
    val rep2 = djset.set(7)
    (djset.union(1, 7).set(1).representative) should equal (djset.set(7).representative)
  }
}
package com.mentisware.disjointset

import com.mentisware.test.UnitSpec

trait ForestSetTestSet {
  def set1 = 1 to 10
}

class ForestSetTest extends UnitSpec with DisjointSetBehavior with ForestSetTestSet {
  "Forest Set" should behave like maintainSetsCorrectly(set1)(ForestSet.apply[Int])

  it should "perform union with weight-union heuristic" in {
    val djset = ForestSet(set1)
    djset.union(1, 2).union(1, 3).union(1, 4).union(7, 8)

    val rep1 = djset.set(1)
    val rep2 = djset.set(7)
    djset.union(1, 7)
    (rep1.representative) should equal (djset.set(7).representative)
  }

}
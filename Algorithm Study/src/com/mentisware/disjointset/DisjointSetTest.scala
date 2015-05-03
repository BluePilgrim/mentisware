package com.mentisware.disjointset

import com.mentisware.test.UnitSpec

trait DisjointSetBehavior { this: UnitSpec =>
  def maintainSetsCorrectly(data: Seq[Int])(buildSet: Seq[Int] => DisjointSet[Int]) {
    val djset = buildSet(data)
    
    it should "create sets whose size equals to original data list" in {
      (data.length) should equal (djset.sets.length)
    }
    
    it should "contain one set after n-1 unions with the first data" in {
      val x = data.head
      
      (djset /: data) { (s, y) =>
        s.union(x, y)
        (s.set(x)) should equal (s.set(y))
        s
      }
      
      (djset.sets.length) should equal (1)
    }
  }
}
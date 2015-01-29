package com.mentisware.advdesign

import com.mentisware.test.UnitSpec

trait TestSet {
  def testSet1 = ("ABCBDAB", "BDCABA", 4)
  def testSet2 = "character"
  def testSet3 = "aa"
  def randomSet(size: Int = 1000) = {
    val rnd = new scala.util.Random(System.currentTimeMillis())
    rnd.nextString(size)
  }
  
  def testSet4 = (Vector(1, 20, 7, 9, 31, 15), 1234)
}

class DPTest extends UnitSpec with TestSet {
  "Longest Common Subsequence" should "find LCS for test set 1" in {
    (DP.getLongestCommonSubsequence(testSet1._1, testSet1._2).length) should equal (testSet1._3)
  }
  
  "Longest Palindrome" should "find longest palindrome list for sample set" in {
    (DP.getLongestPalindrome3(testSet2).length) should equal (DP.getLongestPalindrome2(testSet2).length)
  }

  "Longest Palindrome" should "deal with the even number case" in {
    (DP.getLongestPalindrome3(testSet3).length) should equal (DP.getLongestPalindrome2(testSet3).length)
  }
  
  "Longest Palindrome" should "perform correctly for random string" in {
    val randSet = randomSet(1000)
//    println("str = " + randSet)
    (DP.getLongestPalindrome3(randSet).length) should equal (DP.getLongestPalindrome2(randSet).length)
    
  }
  
  "Coin Change" should "count the minimum coins correctly" in {
    (DP.changeCoins1(testSet4._1, testSet4._2)) should equal (DP.changeCoins2(testSet4._1, testSet4._2))
  }
}
/** Dynamic Programming Exercise
 *
 */
package com.mentisware.advdesign

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

/**
 * @author David
 *
 */
object DP {
  def getLongestCommonSubsequence(s1: String, s2: String): String = {
    // Optimal Substructures
    // if s3(i) is the longest subsequence of s1[m], s2[n],
    // s1(m) == s2(n) => s1(m) == s2(n) == s3(i), and s3(i-1) == LCS(s1[m-1], s2[n-1])
    // s1(m) != s2(n), s1(m) != s3(i) => s3(i) == LCS(s1[m-1], s2[n])
    // s1(m) != s2(n), s2(m) != s3(i) => s3(i) == LCS(s1[m], s2[n-1])
    
    val CSLength = scala.collection.mutable.Map[(Int, Int), Int]()
    val commonSubsequence = new scala.collection.mutable.StringBuilder    
    
    def getLCSLen(s1Index: Int, s2Index: Int): Int = {
      if (CSLength.isDefinedAt((s1Index, s2Index))) CSLength((s1Index, s2Index))
      else {
        if (s1Index < 0 || s2Index < 0) 0
        else if (s1.charAt(s1Index) == s2.charAt(s2Index)) {
          val csLen = getLCSLen(s1Index-1, s2Index-1) + 1
          CSLength += ((s1Index, s2Index) -> csLen)
          csLen
        } else {
          val csLen1 = getLCSLen(s1Index-1, s2Index)
          val csLen2 = getLCSLen(s1Index, s2Index-1)
          val csLen = if (csLen1 > csLen2) csLen1 else csLen2
          CSLength += ((s1Index, s2Index) -> csLen)
          csLen
        }
      }
    }
    
    def generateLCS(s1Index: Int, s2Index: Int, len: Int) {
      if (len > 0 && s1Index >= 0 && s2Index >= 0) {
        val len1 = if (CSLength.isDefinedAt((s1Index-1, s2Index-1))) CSLength((s1Index-1, s2Index-1)) else 0
        val len2 = if (CSLength.isDefinedAt((s1Index-1, s2Index))) CSLength((s1Index-1, s2Index)) else 0
        val len3 = if (CSLength.isDefinedAt((s1Index, s2Index-1))) CSLength((s1Index, s2Index-1)) else 0
        val len = CSLength((s1Index, s2Index))
        
        if (len == len3) generateLCS(s1Index, s2Index-1, len)
        else if (len == len2) generateLCS(s1Index-1, s2Index, len)
        else {
          assert(len == len1 + 1)
          assert(s1.charAt(s1Index) == s2.charAt(s2Index))
          commonSubsequence += s1.charAt(s1Index)
//          println(s1.charAt(s1Index))
          generateLCS(s1Index-1, s2Index-1, len-1)
        }
      }
    }
    
    val LCSLen = getLCSLen(s1.length-1, s2.length-1)
//    println(CSLength)
    generateLCS(s1.length-1, s2.length-1, LCSLen)
    assert(LCSLen == commonSubsequence.length)
    commonSubsequence.reverse.toString
  }
  
  def getLongestPalindrome1(s: String): String = {
    val inputLen = s.length

    val palindromes = scala.collection.mutable.Map[Int, List[(Int, Int, String)]]()
    
    def getPalindromeList(len: Int): List[(Int, Int, String)] = {
      if (palindromes.isDefinedAt(len)) palindromes(len)
      else if (len == 0) {
        val newPList = new scala.collection.mutable.ListBuffer[(Int, Int, String)]
        for (i <- 0 until inputLen) (i, i-1, "") +=: newPList
        val pList = newPList.toList
        palindromes += (0 -> pList)
        pList
      } else if (len == 1) {
        val newPList = new scala.collection.mutable.ListBuffer[(Int, Int, String)]
        for (i <- 0 until inputLen) (i, i, s.charAt(i).toString) +=: newPList
        val pList = newPList.toList
        palindromes += (1 -> pList)
        pList
      } else {
        val pList = getPalindromeList(len-2)
        if (!pList.isEmpty) {
          val newPList = new scala.collection.mutable.ListBuffer[(Int, Int, String)]
        
          pList foreach { p =>
            val (start, end, prevP) = p
            for (i <- start - 1 to 0 by -1;
                  j <- end + 1 until inputLen) {
                  if (s.charAt(i) == s.charAt(j)) {
                    (i, j, s.charAt(i) + prevP + s.charAt(j)) +=: newPList
                  }
            }
          }
          val pList2 = newPList.toList
          palindromes += (len -> pList2)
          pList2
        } else {
          palindromes += (len -> Nil)
          Nil
        }
      }
    }
    
    var longestPLen = inputLen
    while (getPalindromeList(longestPLen).isEmpty) longestPLen -= 1

    println("length = " + longestPLen + " palindrome = " + palindromes(longestPLen).head._3)
    palindromes(longestPLen).map(p =>p._3).head
  }
  
  // redefine the optimal substructure and the recurrence equation.
  // L(i, j) = if (s(i) == s(j)) L(i+1, j-1) + 2
  //           else MAX{ L(i+1, j), L(i, j-1) }
  // base setup : L(i, i) = 1, L(i, i-1) = 0
  // for convenience, the string is assumed to start from 1 to deal with L(i, i-1) case
  def getLongestPalindrome2(s: String): String = {
    val n = s.length
    val L = scala.collection.mutable.ArrayBuffer.fill(n+1, n+1)(0)
    
    // initialize the base cases
    for (i <- 1 to n) L(i)(i) = 1
    
    // calculate maximum length
    for (len <- 2 to n; i <-  n - len + 1 to 1 by -1; j = i + len - 1) {
      if (s.charAt(i-1) == s.charAt(j-1)) {
        L(i)(j) = L(i+1)(j-1) + 2
      } else {
        L(i)(j) = if (L(i+1)(j) >= L(i)(j-1)) L(i+1)(j) else L(i)(j-1)
      }
    }
    
    val longestPLen = L(1)(n)
    
    // find a longest palindrome
    val str = new scala.collection.mutable.StringBuilder
    var p = 1
    var q = n
    while (p < q) {
      if (L(p)(q) == L(p+1)(q)) p += 1
      else if (L(p)(q) == L(p)(q-1)) q -= 1
      else {
        assert(L(p)(q) == L(p+1)(q-1)+2)
        str += s.charAt(p-1); p += 1; q -= 1
      }
    }
    
    val pal = str.toString
    
    val longestPalindrome = if (p == q) pal + s.charAt(p-1) + pal.reverse else pal + pal.reverse

    println("length = " + longestPLen + " palindrome = " + longestPalindrome)
    
    longestPalindrome
  }
  
  def getLongestPalindrome3(s: String): String = {
    val n = s.length
    val L = scala.collection.mutable.Map[(Int, Int), Int]()
    
    def getLPLen(start: Int, end: Int): Int = {
      if (start == end) 1
      else if (start > end) 0
      else if (L.isDefinedAt((start, end))) L((start, end))
      else {
        val LPLen =
          if (s.charAt(start) == s.charAt(end)) getLPLen(start+1, end-1) + 2
          else {
            val len1 = getLPLen(start, end - 1)
            val len2 = getLPLen(start + 1, end)
            if (len1 >= len2) len1 else len2
          }
        L += ((start, end) -> LPLen)
        LPLen
      }
    }
    
    def generateLP(start: Int, end: Int, pal: String): String = {
      if (start == end) pal + s(start) + pal.reverse
      else if (start > end) pal + pal.reverse
      else if (start + 1 == end) {          // remember that L(i, i) does not exist
        if (s(start) == s(end)) generateLP(start, start - 1, pal + s(start))
        else generateLP(start, start, pal)
      } else if (L.isDefinedAt((start, end - 1)) && L((start, end)) == L((start, end - 1))) generateLP(start, end - 1, pal)
      else if (L.isDefinedAt((start + 1, end)) && L((start, end)) == L((start + 1, end))) generateLP(start + 1, end, pal)
      else {
//        println("start = " + start + "(" + s(start) + ")" + " end = " + end + "(" +s(end) + ")")
//        assert(s.charAt(start) == s.charAt(end))
        generateLP(start + 1, end - 1, pal + s.charAt(start))
      }
    }
    
    val LPLen = getLPLen(0, n - 1)
    val palindrome = generateLP(0, n - 1, "")
    println("length = " + LPLen + " palindrome = " + palindrome)
    palindrome
  }
  
  //
  // change coins to minimize the number of coins
  // use the dynamic programming
  // C(v, i) = C(v, i-1) or (C(v-value(i), i) + 1)
  // implement both top-down and bottom-up
  def changeCoins1(coinVals: Vector[Int], amount: Int): Vector[Int] = {
    // the matrix assumes that the index start from 1 and index 0 is used to handle corner cases
    // base cases are
    // C(0, i) = 0, C(x, 0) = max
    val numOfCoins = scala.collection.mutable.ArrayBuffer.tabulate(amount+1, coinVals.length+1) {
      (a, b) => if (b == 0) amount+1 else 0
    }
    
    for (v <- 1 to amount; i <- 1 to coinVals.length) {
      val c1 = numOfCoins(v)(i-1)
      val c2 = if (v >= coinVals(i-1)) numOfCoins(v-coinVals(i-1))(i) + 1 else c1 + 1
      numOfCoins(v)(i) = if (c1 > c2) c2 else c1
    }
    
    // now, numOfCoins(amount)(coins.length) contains the optimal number
    // By tracking numbers in the table, generate the number for each coin
    val coins = ArrayBuffer.fill(coinVals.length)(0)
    var i = coinVals.length
    var v = amount
    
    while (i > 0 && v > 0) {
      if (numOfCoins(v)(i) == numOfCoins(v)(i-1)) {
        i -= 1
//        println("i = " + i)
      } else {
        assert(v < coinVals(i-1) || numOfCoins(v)(i) == numOfCoins(v-coinVals(i-1))(i) + 1)
        coins(i-1) += 1
        v -= coinVals(i-1)
//        println("v = " + v)
      }
    }
    
    println("minimum coin num = " + numOfCoins(amount)(coinVals.length))
    println("coin count = " + coins.toVector)
    coins.toVector
  }
  
  def changeCoins2(coinVals: Vector[Int], amount: Int): Vector[Int] = {
    val numOfCoins = Map[(Int, Int), Int]()
    val coinCount = ArrayBuffer.fill(coinVals.length)(0)
    
    def getNumOfCoins(v: Int, i: Int): Int = {
     if (numOfCoins.isDefinedAt((v, i))) numOfCoins((v, i))
     else if (v == 0) {
       numOfCoins += ((0, i) -> 0)
       0
     } else if (i < 0) {
       numOfCoins += ((v, -1) -> (amount + 1))
       amount + 1
     } else {
       val c1 = getNumOfCoins(v, i-1)
       val c2 = if (v >= coinVals(i)) getNumOfCoins(v - coinVals(i), i) + 1 else c1 + 1
       val num = if (c1 > c2) c2 else c1
       numOfCoins += ((v, i) -> num)
       num
     }
    }
    
    def generateCoinCount(v: Int, i: Int) {
//      println("v = " + v + " i = " + i)
      if (v > 0 && i >= 0) {
        if (numOfCoins((v,i)) == numOfCoins((v,i-1))) {
          generateCoinCount(v, i-1)
        } else {
          assert(v < coinVals(i) || numOfCoins((v,i)) == numOfCoins((v-coinVals(i),i)) + 1)
          coinCount(i) += 1
          generateCoinCount(v-coinVals(i), i)
        }
      }
    }
    
    val coinNum = getNumOfCoins(amount, coinVals.length-1)
    println("minimum coin num = " + coinNum)
    
    generateCoinCount(amount, coinVals.length-1)
    println("coin count = " + coinCount.toVector)
    coinCount.toVector
  }
}
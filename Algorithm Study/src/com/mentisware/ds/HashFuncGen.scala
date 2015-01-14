package com.mentisware.ds

object HashFuncGen {
  private def getPow(n: Int): Int = if (n <= 1) 0 else 1 + getPow(n >> 1)  // 2^p <= n < 2^(P+1)
  private def getNum(p: Int): Long = if (p <= 0) 1 else getNum(p - 1) << 1
    
  def divisionHash(tableSize: Int): (Int => Int) = (key: Int) => key % tableSize

  def multipicationHash(tableSize: Int): (Int => Int) = {
    // h(k) = bottom(m * (kA mod 1))
    val p = getPow(tableSize)
    val filterBitNum = if (tableSize == getNum(p)) p else p + 1
    val s = 2654435769L          // s = A * 2^32
    val filter = (getNum(filterBitNum + 1) - 1) << (32 - filterBitNum)

    (key: Int) => ((((key * s) & filter) >> (32 - filterBitNum)).toInt) % tableSize 
  }

  def universalHash(tableSize: Int): (Int => Int) = {
    // assume that the key's maximum is = 2^31-1
    // tightly upper prime number of the maximum is "2147483659"
    val primeNum = 2147483659L
    
    // generate a, b by using random
    // I use nextInt because the difference between primeNum and maxInt is negligible
    val rnd = new scala.util.Random(System.currentTimeMillis())
    var randomNum = rnd.nextInt()    // any random integer is possible
    while (randomNum == 0) randomNum = rnd.nextInt()
    val a: Long = math.abs(randomNum)
    
    randomNum = rnd.nextInt()
    val b: Long = math.abs(randomNum)
    
    println("[universal hash creation : a = " + a + " b = " + b)
    (key: Int) => (((a * key + b) % primeNum) % tableSize).toInt
  }
}
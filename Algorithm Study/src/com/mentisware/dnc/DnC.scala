package com.mentisware.dnc

object DnC {
  import Ordering.Implicits._
  import Numeric.Implicits._
  
  def getMaxSubSum[T : Numeric](data: Vector[T]): (T, Int, Int) = {
    case class SubSum[T](value: T, start: Int, end: Int)

    def getPartialMaxSubSum(start: Int, end: Int, isLeftPart: Boolean): (SubSum[T], SubSum[T]) = {
      if (start == end) {    // base case
        val subSum = SubSum(data(start), start, end)
        (subSum, subSum)
      } else {
        val mid = (start + end) / 2
        val (leftSubSum, leftAdjSubSum) = getPartialMaxSubSum(start, mid, true)
        val (rightSubSum, rightAdjSubSum) = getPartialMaxSubSum(mid+1, end, false)
        val midSubSum = SubSum(leftAdjSubSum.value + rightAdjSubSum.value,
                                   leftAdjSubSum.start, rightAdjSubSum.end)
        // get max sub sum
        val subSum =
          if (rightSubSum.value < leftSubSum.value && midSubSum.value < leftSubSum.value) leftSubSum
          else if (leftSubSum.value <= rightSubSum.value && midSubSum.value <= rightSubSum.value) rightSubSum
          else midSubSum

        // get max adjacent sub 
        var adjSubSumValue = subSum.value
        val adjSubSum =
          if (isLeftPart) {
            for (i <- subSum.end+1 to end) {
              adjSubSumValue = adjSubSumValue + data(i)
            }
            SubSum(adjSubSumValue, subSum.start, end)
          } else {
            for (i <- start until subSum.start) {
              adjSubSumValue = adjSubSumValue + data(i)
            }
            SubSum(adjSubSumValue, start, subSum.end)
          }
        
        (subSum, adjSubSum)
      }
    }
    val (maxSubSum, maxAdjSubSum) = getPartialMaxSubSum(0, data.length - 1, true)
    (maxSubSum.value, maxSubSum.start, maxSubSum.end)
  }

  def getMaxSubSumLinear[T: Numeric](data: Vector[T]): (T, Int, Int) = {
    case class SubSum[T](value: T, start: Int, end: Int)

    def getOneSidedMaxSubSum(end: Int): (SubSum[T], SubSum[T]) = {
      require(end >= 0)
      if (end == 0) {
        val subSum = SubSum(data(0), 0, 0)
        (subSum, subSum)
      } else {
        val (prevSubSum, prevAdjSubSum) = getOneSidedMaxSubSum(end - 1)
        val newAdjSubSum =
          if (prevAdjSubSum.value + data(end) > data(end))
            SubSum(prevAdjSubSum.value + data(end), prevAdjSubSum.start, end)
          else
            SubSum(data(end), end, end)
        
        if (prevSubSum.value > newAdjSubSum.value) (prevSubSum, newAdjSubSum)
        else (newAdjSubSum, newAdjSubSum)
      }
    }
    val (maxSubSum, maxAdjSubSum) = getOneSidedMaxSubSum(data.length - 1)
    (maxSubSum.value, maxSubSum.start, maxSubSum.end)
  }

// square matrix manipulation to test Strassen's Algorithm
  class Matrix[T: Numeric] (val m: Vector[Vector[T]]) {
    type _Matrix = Vector[Vector[T]]
    val rows = m.length
    val cols = m.head.length

    override def toString = m.toString
    override def equals (other: Any): Boolean = other match {
      case that: Matrix[_] =>
        this.m == that.m && this.rows == that.rows && this.cols == that.cols
    }

    private def _add(m1: _Matrix, m2: _Matrix): _Matrix = {
      (0 until m1.length).toVector.map(row => (m1(row) zip m2(row)).map(t => t._1 + t._2))
    }
    
    def add(other: Matrix[T]): Matrix[T] = {
      require(rows == other.rows && cols == other.cols)
      new Matrix(_add(m, other.m))
    }
    
    private def _sub(m1: _Matrix, m2: _Matrix): _Matrix = {
      (0 until m1.length).toVector.map(row => (m1(row) zip m2(row)).map(t => t._1 - t._2))      
    }
    
    def sub(other: Matrix[T]): Matrix[T] = {
      require(rows == other.rows && cols == other.cols)
      new Matrix(_sub(m, other.m))      
    }
    
    private def unitProd(m1: _Matrix, m2: _Matrix, row: Int, col: Int) = {
      def multSum(k: Int): T = if (k == 0) m1(row)(0) * m2(0)(col) else m1(row)(k) * m2(k)(col) + multSum(k - 1)
      multSum(m1.head.length - 1)
    }
    
    private def _production(m1: _Matrix, m2: _Matrix): _Matrix = {
      val (rows, cols) = (m1.length, m2.head.length)
      
      (0 until rows).toVector.map(row => (0 until cols).toVector.map(col => unitProd(m1, m2, row, col)))
    }
    
    def production0(other: Matrix[T]): Matrix[T] = {
      require(this.cols == other.rows)
      new Matrix(_production(this.m, other.m))
    }

    
    // assume n is power of 2
    private def doStrassen(m1: _Matrix, m2: _Matrix, n: Int): _Matrix = {
      import scala.collection.mutable.ArraySeq

      /* util operations on input matrices in half size without copying */
      def addOnInput(A: (_Matrix, Int, Int), B: (_Matrix, Int, Int)): _Matrix = {
        (0 until n/2).toVector.map(row =>
          (0 until n/2).toVector.map(col => A._1(A._2+row)(A._3+col) + B._1(B._2+row)(B._3+col)))
      }
        
      def subOnInput(A: (_Matrix, Int, Int), B: (_Matrix, Int, Int)): _Matrix = {
        (0 until n/2).toVector.map(row =>
          (0 until n/2).toVector.map(col => A._1(A._2+row)(A._3+col) - B._1(B._2+row)(B._3+col)))
      }
      
      def getSubMatrix(m: _Matrix, rowOffset: Int, colOffset: Int): _Matrix = {
        (0 until n/2).toVector.map(row =>
          (0 until n/2).toVector.map(col => m(rowOffset+row)(colOffset+col)))
      }
        
      if (n == 1) 
        Vector(Vector(m1(0)(0) * m2(0)(0)))
      else {
        val half = n / 2
        
        /* Sum or Difference to create util matrices */
        // S1 = B12 - B22
        val s1 = subOnInput((m2, 0, half), (m2, half, half))
        // S2 = A11 + A12
        val s2 = addOnInput((m1, 0, 0), (m1, 0, half))
        // S3 = A21 + A22
        val s3 = addOnInput((m1, half, 0), (m1, half, half))
        // S4 = B21 - B11
        val s4 = subOnInput((m2, half, 0), (m2, 0, 0))
        // S5 = A11 + A22
        val s5 = addOnInput((m1, 0, 0), (m1, half, half))
        // S6 = B11 + B22
        val s6 = addOnInput((m2, 0, 0), (m2, half, half))
        // S7 = A12 - A22
        val s7 = subOnInput((m1, 0, half), (m1, half, half))
        // S8 = B21 + B22
        val s8 = addOnInput((m2, half, 0), (m2, half, half))
        // S9 = A11 - A21
        val s9 = subOnInput((m1, 0, 0), (m1, half, 0))
        // S10 = B11 + B12
        val s10 = addOnInput((m2, 0, 0), (m2, 0, half))
        
        /* Productions to create intermediate matrices */
        // P1 = A11 * S1
        val p1 = doStrassen(getSubMatrix(m1, 0, 0), s1, half)
        // P2 = S2 * B22
        val p2 = doStrassen(s2, getSubMatrix(m2, half, half), half)
        // P3 = S3 * B11
        val p3 = doStrassen(s3, getSubMatrix(m2, 0, 0), half)
        // P4 = A22* S4
        val p4 = doStrassen(getSubMatrix(m1, half, half), s4, half)
        // P5 = S5 * S6
        val p5 = doStrassen(s5, s6, half)
        // P6 = S7 * S8
        val p6 = doStrassen(s7, s8, half)
        // P7 = S9 * S10
        val p7 = doStrassen(s9, s10, half)
        
        /* calculate sub results */
        // C11 = P5 + P4 - P2 + P6
        val c11 = _add(_sub(_add(p5, p4), p2), p6)
        // C12 = P1 + P2
        val c12 = _add(p1, p2)
        // C21 = P3 + P4
        val c21 = _add(p3, p4)
        // C22 = P5 + P1 - P3 - P7
        val c22 = _sub(_sub(_add(p5, p1), p3), p7)
        
        /* integrate the result into one matrix */
        (c11 zip c12).map(t => t._1 ++ t._2) ++ (c21 zip c22).map(t => t._1 ++ t._2)
      }
    }

    private def expandWithZeroPad(n: Int): _Matrix = {
      val z = implicitly[Numeric[T]].zero

      if (rows == n && cols == n)
        m
      else
        (0 until rows).toVector.map(m(_).padTo(n, z)) ++
          (rows until n).toVector.map(_ => Vector.fill(n)(z))
    }
      

    def production(other: Matrix[T]): Matrix[T] = {
      require(this.cols == other.rows)    // (a * x) prod (x * b)

      /* let's get the tight outer-bounding square matrix cohsidering all dimensions */
      def getPower(n: Int): Int = if (n == 1) 0 else getPower(n >> 1) + 1      
      val maxSize =
        if (this.rows > this.cols && this.rows > other.cols) this.rows
        else if (this.cols > this.rows && this.cols > other.cols) this.cols
        else other.cols
      val _n = 1 << getPower(maxSize)
      val n = if (maxSize == _n) _n else _n << 1

      /* apply Strassen's method after padding with zero for bounding squire matrix */
      val prod = doStrassen(this.expandWithZeroPad(n), other.expandWithZeroPad(n), n)
 
      /* extract the result of (a * b) */
      new Matrix((0 until this.rows).toVector.map(prod(_).dropRight(n - other.cols)))
    }
  }
}
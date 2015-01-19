package com.mentisware.advdesign

import Numeric.Implicits._


// square matrix manipulation to test Strassen's Algorithm
case class Matrix[T: Numeric] (val m: Vector[Vector[T]]) {
  type _Matrix = Vector[Vector[T]]
  val rows = m.length
  val cols = m.head.length

  override def toString = m.toString
  override def equals (other: Any): Boolean = other match {
    case that: Matrix[_] =>
      this.m == that.m && this.rows == that.rows && this.cols == that.cols
  }

  private def _add(m1: _Matrix, m2: _Matrix): _Matrix =
    (m1 zip m2).map { row =>
      (row._1 zip row._2).map(e => e._1 + e._2)
    }

  def add(that: Matrix[T]): Matrix[T] = {
    require(rows == that.rows && cols == that.cols)
    
    Matrix(_add(this.m, that.m))
  }
  
  private def _sub(m1: _Matrix, m2: _Matrix): _Matrix =
    (m1 zip m2).map { row =>
      (row._1 zip row._2).map(e => e._1 - e._2)
    }

  def sub(that: Matrix[T]): Matrix[T] = {
    require(rows == that.rows && cols == that.cols)
    Matrix(_sub(this.m, that.m))
  }
  
  def transpose(): Matrix[T] = Matrix((0 until cols).toVector.map(col => m.map(_(col))))
  
  def production0(that: Matrix[T]): Matrix[T] = {
    require(cols == that.rows)
    
    // for A * B = C
    // C(i,j) = sum(A(i,k) + B(k, j))
    def unitProd(a: _Matrix, b: _Matrix, i: Int, j: Int): T =
      (a(i) zip b.map(_(j))).map(e => e._1 * e._2).reduceLeft(_+_)

    Matrix {
      (0 until rows).toVector.map { row =>
        (0 until that.cols).toVector.map(col => unitProd(this.m, that.m, row, col))
      }
    }
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

    /* let's get the tight outer-bounding square matrix considering all dimensions */
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
    Matrix(prod.slice(0, rows).map(_.slice(0, other.cols)))
  }
}


object Matrix {
  def findOptimalProductionSequence[T](mats: Vector[Matrix[T]]): Map[(Int, Int), Int] = {
    val matNum = mats.length
    val p: Vector[Int] = {               // mats(i) has p(i) * p(i+1) dimension.
                                                  // p(0) = mats(0).rows, p(1) = mats(0).cols
      (Vector(mats(0).rows) /: mats) (_ :+ _.cols)
    }

    // check the productability
    def productable(i: Int, b: Boolean): Boolean =
      if (i == 0) b && mats(0).rows == p(0)
      else b && productable(i-1, mats(i).rows == p(i))
      

    productable(matNum-1, true)

//    import scala.collection.mutable.Map
    
    // calculate the optimal production sequence through dynamic programming
    // M(i, ..., j) in mats, the optimal production cost c(i, j) = min(c(i, k) + c(k+1, j) + p(i)p(k+1)p(j+1))

    // bottom-up approach
    val cost = scala.collection.mutable.Map[(Int, Int), Int]()
    val split = scala.collection.mutable.Map[(Int, Int), Int]()

    for (i <- 0 until matNum) {
      cost += ((i, i) -> 0)      // cost from 1-matrix sequence is zero.
      split += ((i, i) -> i)
    }
    
    for (seqSize <- 2 to matNum; i <- 0 to matNum - seqSize; j = i + seqSize - 1) {
      var minC = cost((i+1, j)) + p(i) * p(i+1) * p(j+1)
      var s = i
      for (k <- i + 1 until j ) {
        val c = cost((i, k)) + cost((k+1, j)) + p(i) * p(k+1) * p(j+1)
        if (c < minC) {
          minC = c
          s = k
        }
      }
      cost += ((i, j) -> minC)
      split += ((i, j) -> s)
    }

//    println("cost table = " + cost)
//    println("split table = " + split)
    split.toMap
  }
  
  def chainProduction[T](mats: Vector[Matrix[T]]): Matrix[T] = {
    val split = findOptimalProductionSequence(mats)

    def guidedProduction(i: Int, j: Int): Matrix[T] = {
      if (i == j) mats(i)
      else if (i + 1 == j) mats(i) production mats(j)
      else {
        val s = split((i, j))
        guidedProduction(i, s) production guidedProduction(s+1, j)
      }
    }
    
    guidedProduction(0, mats.length-1)
  }
  
}
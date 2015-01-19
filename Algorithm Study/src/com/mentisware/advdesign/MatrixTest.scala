package com.mentisware.advdesign

import com.mentisware.test.UnitSpec


trait MatrixBehaviors[T] { this: UnitSpec =>
  def addition(m1: => Matrix[T], m2: => Matrix[T], result: => Matrix[T]) {
    it should "have same dimensions [add]" in {
      (m1.rows) should equal (m2.rows)
      (m1.cols) should equal (m2.cols)
    }
    
    it should "work correct [add]" in {
      (m1 add m2) should equal (result)
    }
  }
  
  def subtraction(m1: => Matrix[T], m2: => Matrix[T], result: => Matrix[T]) {
    it should "have same dimensions [sub]" in {
      (m1.rows) should equal (m2.rows)
      (m1.cols) should equal (m2.cols)
    }
    
    it should "work correct [sub]" in {
      (m1 sub m2) should equal (result)
    }    
  }
  
  def production(m1: => Matrix[T], m2: => Matrix[T], result: => Matrix[T]) {
    it should "have matching column and row [production]" in {
      (m1.cols) should equal (m2.rows)
    }
    
    it should "work correct correct [production]" in {
      (m1 production m2) should equal (result)
    }
  }
  
  def chainProduction(mats: => Vector[Matrix[T]], result: => Map[(Int, Int), Int]) {
    def checkSplitResult(split: Map[(Int, Int), Int]): Boolean = {
      def _checkResult(i: Int, j: Int, b: Boolean): Boolean = {
        if (i+1 >= j) b
        else {
          val s = split((i, j))
          val res = s == result((i, j))
          b && _checkResult(i, s, res) && _checkResult(s+1, j, res)
        }
      }
      
      _checkResult(0, mats.length-1, true)
    }
    
    it should "find optimal split points" in {
      val split = Matrix.findOptimalProductionSequence(mats)
      
      (checkSplitResult(split)) should equal (true)
    }
    
    it should "perform chain production correctly" in {
      Matrix.chainProduction(mats)
    }
  }
}

class MatrixTest extends UnitSpec with MatrixBehaviors[Int] {
  def power2SquareMatrixSet1: (Matrix[Int], Matrix[Int], Matrix[Int], Matrix[Int], Matrix[Int]) = {
    val m1 = Matrix(Vector(Vector(2, 1), Vector(-1, 2)))
    val m2 = Matrix(Vector(Vector(0, 1), Vector(1, 1)))
    val addR = Matrix(Vector(Vector(2, 2), Vector(0, 3)))
    val subR = Matrix(Vector(Vector(2, 0), Vector(-2, 1)))
    val prodR = m1 production0 m2
    (m1, m2, addR, subR, prodR)
  }
  
  def power2SquareMatrixSet2: (Matrix[Int], Matrix[Int], Matrix[Int]) = {
    val m1 = Matrix(Vector(Vector(1, 2, 3, 0), Vector(-1, 2, 3, 1), Vector(1, 0, 0, -1), Vector(-2, 1, 2, 1)))
    val m2 = Matrix(Vector(Vector(-2, 3, 1, -1), Vector(1, 1, 2, 1), Vector(0, 1, 2, -1), Vector(2, 1, 1, 2)))
    val m3 = m1 production0 m2
    (m1, m2, m3)
  }

  def nonPower2SquareMatrixSet: (Matrix[Int], Matrix[Int], Matrix[Int]) = {
    val m1 = Matrix(Vector(Vector(1, 2, 3), Vector(-2, 0, 1), Vector(1, 4, 2)))
    val m2 = Matrix(Vector(Vector(-1, 1, 1), Vector(2, 1, 1), Vector(-1, -1, 0)))
    val m3 = m1 production0 m2
    (m1, m2, m3)
  }
  
  def normalMatrixSet: (Matrix[Int], Matrix[Int], Matrix[Int]) = {
    val m1 = new Matrix(Vector(Vector(1, 2, 3), Vector(-1, 2, 1)))
    val m2 = new Matrix(Vector(Vector(-2, 1), Vector(1, 0), Vector(1, 1)))
    val m3 = m1 production0 m2
    (m1, m2, m3)
  }

  def createMatrix(rows: Int, cols: Int): Matrix[Int] = {
    val rnd = new scala.util.Random(System.currentTimeMillis())
    Matrix(Vector.fill(rows, cols)(rnd.nextInt(100) - 50))
  }

  def matrixSequence1 = {
    val seq = Vector(createMatrix(30, 35),
              createMatrix(35, 15),
              createMatrix(15, 5),
              createMatrix(5, 10),
              createMatrix(10, 20),
              createMatrix(20, 25)
             )
             
  // Originally in CLR book,
  // (1, 6)->3, (1, 3)->1, (4, 6)->5, (2, 3)->2, (4, 5)->4, (5, 6)->5
    val split = Map((0, 5) -> 2, (0, 2) -> 0, (3, 5) -> 4, (1, 2) -> 1, (3, 4) -> 3, (4, 5) -> 4)
    (seq, split)
  }
  
  val set1 = power2SquareMatrixSet1
  
  "A Power2 Square Matrix Set1" should behave like addition(set1._1, set1._2, set1._3)
  it should behave like subtraction(set1._1, set1._2, set1._4)
  it should behave like production(set1._1, set1._2, set1._5)
  
  val set2 = power2SquareMatrixSet2
  "A Power2 Square Matrix Set2" should behave like production(set2._1, set2._2, set2._3)
  
  val set3 = nonPower2SquareMatrixSet
  "A Non-Power2 Sqaure Matrix Set" should behave like production(set3._1, set3._2, set3._3)
  
  val set4 = normalMatrixSet
  "A Normal Matrix Set" should behave like production(set4._1, set4._2, set4._3)
  
  val set5 = matrixSequence1
  "A sequence of matrix" should behave like chainProduction(set5._1, set5._2)

}
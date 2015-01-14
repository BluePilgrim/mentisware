package com.mentisware.dnc

import com.mentisware.test.UnitSpec
import com.mentisware.dnc.DnC.Matrix

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
}

class DnCTest extends UnitSpec with MatrixBehaviors[Int] {
  def power2SquareMatrixSet1: (Matrix[Int], Matrix[Int], Matrix[Int], Matrix[Int], Matrix[Int]) = {
    val m1 = new Matrix(Vector(Vector(2, 1), Vector(-1, 2)))
    val m2 = new Matrix(Vector(Vector(0, 1), Vector(1, 1)))
    val addR = new Matrix(Vector(Vector(2, 2), Vector(0, 3)))
    val subR = new Matrix(Vector(Vector(2, 0), Vector(-2, 1)))
    val prodR = m1 production0 m2
    (m1, m2, addR, subR, prodR)
  }
  
  def power2SquareMatrixSet2: (Matrix[Int], Matrix[Int], Matrix[Int]) = {
    val m1 = new Matrix(Vector(Vector(1, 2, 3, 0), Vector(-1, 2, 3, 1), Vector(1, 0, 0, -1), Vector(-2, 1, 2, 1)))
    val m2 = new Matrix(Vector(Vector(-2, 3, 1, -1), Vector(1, 1, 2, 1), Vector(0, 1, 2, -1), Vector(2, 1, 1, 2)))
    val m3 = m1 production0 m2
    (m1, m2, m3)
  }

  def nonPower2SquareMatrixSet: (Matrix[Int], Matrix[Int], Matrix[Int]) = {
    val m1 = new Matrix(Vector(Vector(1, 2, 3), Vector(-2, 0, 1), Vector(1, 4, 2)))
    val m2 = new Matrix(Vector(Vector(-1, 1, 1), Vector(2, 1, 1), Vector(-1, -1, 0)))
    val m3 = m1 production0 m2
    (m1, m2, m3)
  }
  
  def normalMatrixSet: (Matrix[Int], Matrix[Int], Matrix[Int]) = {
    val m1 = new Matrix(Vector(Vector(1, 2, 3), Vector(-1, 2, 1)))
    val m2 = new Matrix(Vector(Vector(-2, 1), Vector(1, 0), Vector(1, 1)))
    val m3 = m1 production0 m2
    (m1, m2, m3)
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
}
package org.vro.day04

import org.scalatest.Assertions
import org.scalatest.matchers.should.Matchers

object Runner extends Assertions with Matchers {

  def containsDoubleOrLonger(i: Int): Boolean = {
    i.toString.sliding(2).exists(p => p(0).equals(p(1)))
  }

  def isIncreasing(i: Int): Boolean = {
    i.toString.sliding(2).forall(p => p(0) <= p(1))
  }

  def containsDoubleButNotLonger(i: Int): Boolean = {
    val triples = i.toString.sliding(3).filter(t => t(0).equals(t(1)) && t(0).equals(t(2))).toList

    i.toString.sliding(2)
      .exists(d => d(0).equals(d(1)) && triples.forall(t => !t(0).equals(d(0))))
  }

  def isValid(i: Int): Boolean = {
    containsDoubleOrLonger(i) && isIncreasing(i)
  }

  def isReallyValid(i: Int): Boolean = {
    containsDoubleButNotLonger(i) && isIncreasing(i)
  }

  def part_1(a: Int, b: Int): Int = {
    a.to(b).count(i => isValid(i))
  }

  def part_2(a: Int, b: Int): Int = {
    a.to(b).count(i => isReallyValid(i))
  }

  def main(args: Array[String]) {
    test()
    println("Part #1: " + part_1(272091, 815432))
    println("Part #2: " + part_2(272091, 815432))
  }

  def test(): Unit = {
    containsDoubleOrLonger(122345) shouldBe true

    isValid(111111) shouldBe true
    isValid(111123) shouldBe true
    isValid(135679) shouldBe false
    isValid(122345) shouldBe true
    isValid(223450) shouldBe false

    containsDoubleButNotLonger(123444) shouldBe false
    containsDoubleButNotLonger(111122) shouldBe true

    isReallyValid(112233) shouldBe true
    isReallyValid(123444) shouldBe false
    isReallyValid(111122) shouldBe true

  }
}
package org.vro.day01

import org.scalatest.Assertions
import org.scalatest.matchers.should.Matchers

import scala.io.Source

object Runner extends Assertions with Matchers {

  def getFuel(mass: Int) : Int = {
    (mass/3)-2
  }

  def getTotalFuel(mass: Int, sum: Int = 0): Int = {
    (mass / 3) - 2 match {
      case fuel if fuel > 0 => getTotalFuel(fuel, sum + fuel)
      case _ => sum
    }
  }

  def main(args: Array[String]) {
    test()
    val input = Source.fromFile("input.txt").getLines().toList
    println(input.map(x => getFuel(x.toInt)).sum)
    println(input.map(x => getTotalFuel(x.toInt)).sum)
  }

  def test(): Unit = {
    getFuel(12) shouldBe 2
    getFuel(14) shouldBe 2
    getFuel(1969) shouldBe 654
    getFuel(100756) shouldBe 33583

    getTotalFuel(14) shouldBe 2
    getTotalFuel(1969) shouldBe 966
    getTotalFuel(100756) shouldBe 50346
  }

}
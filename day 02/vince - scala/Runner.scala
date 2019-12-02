package org.vro.day02

import org.scalatest.Assertions
import org.scalatest.matchers.should.Matchers

import scala.io.Source

import resource._

object Runner extends Assertions with Matchers {

  def part_1(input: List[Int], idx: Int = 0): List[Int] = {
    input(idx) match {
      case 1 => // +
        part_1(input.updated(input(idx + 3), input(input(idx + 1)) + input(input(idx + 2))), idx + 4)
      case 2 => // *
        part_1(input.updated(input(idx + 3), input(input(idx + 1)) * input(input(idx + 2))), idx + 4)
      case _ => input
    }
  }

  def main(args: Array[String]) {
    test()
    for (r <- managed(Source.fromFile("input.txt"))) {
      val input = r.mkString.split(",").map(_.toInt).toList
      println("Part #1: " + part_1(input.updated(1, 12).updated(2, 2)).head)
      println("Part #2 - (0, 0): " + part_1(input.updated(1, 0).updated(2, 0)).head)
      println("Part #2 - (1, 0): " + part_1(input.updated(1, 1).updated(2, 0)).head)
      println("Part #2 - (2, 0): " + part_1(input.updated(1, 2).updated(2, 0)).head)
      println("Part #2 - (3, 0): " + part_1(input.updated(1, 3).updated(2, 0)).head)
      println("Part #2 - (1, 1): " + part_1(input.updated(1, 3).updated(2, 1)).head)
      println("Part #2 - (1, 2): " + part_1(input.updated(1, 3).updated(2, 2)).head)
      println("Part #2 - (1, 3): " + part_1(input.updated(1, 3).updated(2, 3)).head)
      println("Part #2 - Noun: " + (19690720 - 797822) / (1028222 - 797822))
      println("Part #2 - Verb: " + (19690720 - 797822) % (1028222 - 797822))
      println("Part #2 - (82, 98): " + part_1(input.updated(1, 82).updated(2, 98)).head)
    }
  }

  def test(): Unit = {
    part_1(List(1, 0, 0, 0, 99)) shouldBe List(2, 0, 0, 0, 99)
    part_1(List(2, 3, 0, 3, 99)) shouldBe List(2, 3, 0, 6, 99)
    part_1(List(2, 4, 4, 5, 99, 0)) shouldBe List(2, 4, 4, 5, 99, 9801)
    part_1(List(1, 1, 1, 4, 99, 5, 6, 0, 99)) shouldBe List(30, 1, 1, 4, 2, 5, 6, 0, 99)
  }

}
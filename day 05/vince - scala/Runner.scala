package org.vro.day05

import org.scalatest.Assertions
import org.scalatest.matchers.should.Matchers

import scala.io.Source

import resource._

object Runner extends Assertions with Matchers {

  def part_1(input: List[Int], idx: Int = 0, manualInput: List[Int], output: List[Int]= List()) : List[Int] = {
    input(idx).toString.takeRight(2).toInt match {
      case 1 => // +
        val (a, b) = getTwoParams(idx, input)
        part_1(input.updated(input(idx + 3), a + b), idx + 4, manualInput, output)
      case 2 => // *
        val (a, b) = getTwoParams(idx, input)
        part_1(input.updated(input(idx + 3), a * b), idx + 4, manualInput, output)
      case 3 => // input
        part_1(input.updated(input(idx + 1), manualInput.head), idx + 2, manualInput.tail, output)
      case 4 => // output
        // println(getOneParam(idx, input))
        part_1(input, idx + 2, manualInput, output :+ getOneParam(idx, input))
      case 5 =>
        val (a, b) = getTwoParams(idx, input)
        part_1(input, if (a != 0) b else idx + 3, manualInput, output)
      case 6 =>
        val (a, b) = getTwoParams(idx, input)
        part_1(input, if (a == 0) b else idx + 3, manualInput, output)
      case 7 =>
        val (a, b) = getTwoParams(idx, input)
        val res = if (a < b) 1 else 0
        part_1(input.updated(input(idx + 3), res), idx + 4, manualInput, output)
      case 8 =>
        val (a, b) = getTwoParams(idx, input)
        val res = if (a == b) 1 else 0
        part_1(input.updated(input(idx + 3), res), idx + 4, manualInput, output)
      case _ =>
        output
    }
  }

  def getOneParam(idx: Int, input: List[Int]) : Int = {
    val modes = input(idx).toString.dropRight(2).reverse.padTo(1, 0).reverse.mkString("")
    readParam(idx + 1, modes.last, input)
  }

  def getTwoParams(idx: Int, input: List[Int]) : (Int, Int) = {
    val modes = input(idx).toString.dropRight(2).reverse.padTo(2, 0).reverse.mkString("")
    (readParam(idx + 1, modes.last, input), readParam(idx + 2, modes.dropRight(1).last, input))
  }

  def readParam(idx: Int, mode: Int, input: List[Int]): Int = {
    mode match {
      case '1' => input(idx)
      case _ => input(input(idx))
    }
  }

  def main(args: Array[String]) {
    test()
    for (r <- managed(Source.fromFile("input.txt"))) {
      val input = r.mkString.split(",").map(_.toInt).toList
      println(part_1(input, 0, List(1)))
      println(part_1(input, 0, List(5)))
    }
  }

  def test(): Unit = {
  }
}
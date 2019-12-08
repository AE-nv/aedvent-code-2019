package org.vro.day07

import org.scalatest.Assertions
import org.scalatest.matchers.should.Matchers

import scala.io.Source
import resource._

import scala.collection.mutable

object Runner extends Assertions with Matchers {

  case class Program(input: List[Int]
                     , idx: Int
                     , manualInput: List[Int]
                     , output: List[Int] = List()
                     , haltOnOutput: Boolean = true
                     , running: Boolean = true) {

    def exec(): Program = {
      input(idx).toString.takeRight(2).toInt match {
        case 1 => // +
          val (a, b) = getTwoParams(idx, input)
          Program(input.updated(input(idx + 3), a + b), idx + 4, manualInput, output).exec()
        case 2 => // *
          val (a, b) = getTwoParams(idx, input)
          Program(input.updated(input(idx + 3), a * b), idx + 4, manualInput, output).exec()
        case 3 => // input
          Program(input.updated(input(idx + 1), manualInput.head), idx + 2, manualInput.tail, output).exec()
        case 4 => // output
          if (haltOnOutput) {
            Program(input, idx + 2, manualInput, output :+ getOneParam(idx, input))
          } else {
            Program(input, idx + 2, manualInput, output :+ getOneParam(idx, input)).exec()
          }
        case 5 =>
          val (a, b) = getTwoParams(idx, input)
          Program(input, if (a != 0) b else idx + 3, manualInput, output).exec()
        case 6 =>
          val (a, b) = getTwoParams(idx, input)
          Program(input, if (a == 0) b else idx + 3, manualInput, output).exec()
        case 7 =>
          val (a, b) = getTwoParams(idx, input)
          val res = if (a < b) 1 else 0
          Program(input.updated(input(idx + 3), res), idx + 4, manualInput, output).exec()
        case 8 =>
          val (a, b) = getTwoParams(idx, input)
          val res = if (a == b) 1 else 0
          Program(input.updated(input(idx + 3), res), idx + 4, manualInput, output).exec()
        case _ =>
          Program(input, idx, manualInput, output, running = false)
      }
    }
  }

  def getOneParam(idx: Int, input: List[Int]): Int = {
    val modes = input(idx).toString.dropRight(2).reverse.padTo(1, 0).reverse.mkString("")
    readParam(idx + 1, modes.last, input)
  }

  def getTwoParams(idx: Int, input: List[Int]): (Int, Int) = {
    val modes = input(idx).toString.dropRight(2).reverse.padTo(2, 0).reverse.mkString("")
    (readParam(idx + 1, modes.last, input), readParam(idx + 2, modes.dropRight(1).last, input))
  }

  def readParam(idx: Int, mode: Int, input: List[Int]): Int = {
    mode match {
      case '1' => input(idx)
      case _ => input(input(idx))
    }
  }

  def amplifier(program: List[Int], signals: List[Int]): Int = {

    val o1 = Program(program, 0, List(signals.head, 0), haltOnOutput = false).exec().output.head
    val o2 = Program(program, 0, List(signals(1), o1), haltOnOutput = false).exec().output.head
    val o3 = Program(program, 0, List(signals(2), o2), haltOnOutput = false).exec().output.head
    val o4 = Program(program, 0, List(signals(3), o3), haltOnOutput = false).exec().output.head
    val o5 = Program(program, 0, List(signals(4), o4), haltOnOutput = false).exec().output.head

    o5
  }

  def amplifierV2(program: List[Int], signals: List[Int]): Int = {

    val amplifiers = mutable.HashMap[Int, Program]()
    amplifiers(0) = Program(program, 0, List(signals.head, 0)).exec()
    amplifiers(1) = Program(program, 0, List(signals(1), amplifiers(0).output.head)).exec()
    amplifiers(2) = Program(program, 0, List(signals(2), amplifiers(1).output.head)).exec()
    amplifiers(3) = Program(program, 0, List(signals(3), amplifiers(2).output.head)).exec()
    amplifiers(4) = Program(program, 0, List(signals(4), amplifiers(3).output.head)).exec()

    amplifiers(0) = Program(amplifiers(0).input, amplifiers(0).idx, List(amplifiers(4).output.head)).exec()

    while (amplifiers(0).running) {
      amplifiers(1) = Program(amplifiers(1).input, amplifiers(1).idx, List(amplifiers(0).output.head)).exec()
      amplifiers(2) = Program(amplifiers(2).input, amplifiers(2).idx, List(amplifiers(1).output.head)).exec()
      amplifiers(3) = Program(amplifiers(3).input, amplifiers(3).idx, List(amplifiers(2).output.head)).exec()
      amplifiers(4) = Program(amplifiers(4).input, amplifiers(4).idx, List(amplifiers(3).output.head)).exec()
      amplifiers(0) = Program(amplifiers(0).input, amplifiers(0).idx, List(amplifiers(4).output.head)).exec()
    }

    amplifiers(4).output.head
  }

  def parseInput(fileName: String): List[Int] = {
    val ret = for (r <- managed(Source.fromFile(fileName));
                   input = r.mkString.split(",").map(_.toInt).toList) yield input
    ret.opt.getOrElse(List())
  }

  def main(args: Array[String]) {
    test()
    println("part #1: " + (0 to 4).toList.permutations.map(s => amplifier(parseInput("input.txt"), s)).max)
    println("part #2: " + (5 to 9).toList.permutations.map(s => amplifierV2(parseInput("input.txt"), s)).max)
  }

  def test(): Unit = {
    amplifier(parseInput("example1.txt"), List(4, 3, 2, 1, 0)) shouldBe 43210
    amplifier(parseInput("example2.txt"), List(0, 1, 2, 3, 4)) shouldBe 54321
    amplifier(parseInput("example3.txt"), List(1, 0, 4, 3, 2)) shouldBe 65210
    amplifierV2(parseInput("example4.txt"), List(9, 8, 7, 6, 5)) shouldBe 139629729
    amplifierV2(parseInput("example5.txt"), List(9, 7, 8, 5, 6)) shouldBe 18216
  }
}
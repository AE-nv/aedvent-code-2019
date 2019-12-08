package org.vro.day08

import org.scalatest.Assertions
import org.scalatest.matchers.should.Matchers
import resource._

import scala.io.Source

object Runner extends Assertions with Matchers {

  def part_1(fileName: String, w: Int, h: Int): Int = {
    val layers = getInputFromFile(fileName).grouped(w * h)
    val layerWithFewest0 = layers.minBy(l => l.count(_.equals('0')))
    layerWithFewest0.count(_.equals('1')) * layerWithFewest0.count(_.equals('2'))
  }

  def getInputFromFile(fileName: String): String = {
    val ret = for (r <- managed(Source.fromFile(fileName));
                   inputString = r.mkString) yield inputString
    ret.opt.getOrElse("")
  }

  def overlay(p1: Char, p2: Char): Char = {
    if (p1.equals('2')) p2 else p1
  }

  def part_2(input: String, w: Int, h: Int): String = {
    input.grouped(w * h).reduce((l1, l2) => l1.zip(l2).map(p => overlay(p._1, p._2)).mkString)
  }

  def showLayer(layer: String, w: Int): Unit = {
    layer.map(c => if (c.equals('0')) ' ' else '▓').grouped(w).foreach(println)
  }

  def main(args: Array[String]) {
    test()
    println("part #1: " + part_1("input.txt", 25, 6))
    val decoded = part_2(getInputFromFile("input.txt"), 25, 6)
    println("part #2: ")
    println()
    showLayer(decoded, 25)
  }

  def test(): Unit = {
    part_2("0222112222120000", 2, 2) shouldBe "0110"
  }
}
package org.vro.day06

import org.scalatest.Assertions
import org.scalatest.matchers.should.Matchers
import resource._

import scala.io.Source

object Runner extends Assertions with Matchers {

  case class Node(node: String, children: List[Node], parent: Option[Node] = None) {
    def orbits(d: Int = 0): Int = if (children.isEmpty) d else d + children.map(c => c.orbits(d + 1)).sum
  }

  def buildTree(input: Map[String, List[String]], k: String): Node = {
    if (input.get(k).isEmpty) {
      Node(k, List())
    } else {
      Node(k, input(k).map(l => buildTree(input, l)))
    }
  }

  def parseInputToTuples(fileName: String): List[(String, String)] = {
    val m = for (r <- managed(Source.fromFile(fileName));
                 m = r.getLines().toList.map(a => a.split("\\)")).map(a => (a(0), a(1)))
    ) yield m
    m.opt.getOrElse(List())
  }

  def part_1(fileName: String): Int = {
    val tree = buildTree(parseInputToTuples(fileName).groupBy(a => a._1).mapValues(l => l.map(i => i._2)), "COM")
    tree.orbits()
  }

  def pathToRoot(source: String, input: Map[String, String], acc: List[String] = List()): List[String] = {
    input.get(source) match {
      case x if x.nonEmpty => pathToRoot(x.get, input, x.get +: acc)
      case _ => acc
    }
  }

  def removeCommonPrefix(a: List[String], b: List[String]): (List[String], List[String]) = {
    (a.headOption, b.headOption) match {
      case (x, y) if x.isDefined && x.equals(y) => removeCommonPrefix(a.tail, b.tail)
      case _ => (a, b)
    }
  }

  def part_2(fileName: String): Int = {
    val input = parseInputToTuples(fileName).map(_.swap).toMap
    val (a, b) = removeCommonPrefix(pathToRoot("YOU", input), pathToRoot("SAN", input))
    a.length + b.length
  }

  def main(args: Array[String]) {
    test()
    println("part #1: " + part_1("input.txt"))
    println("part #2: " + part_2("input.txt"))
  }

  def test(): Unit = {
    part_1("example.txt") shouldBe 42
    removeCommonPrefix(List("A"), List()) shouldBe(List("A"), List())
    removeCommonPrefix(List("A"), List("B")) shouldBe(List("A"), List("B"))
    removeCommonPrefix(List("A", "B"), List("A")) shouldBe(List("B"), List())
    removeCommonPrefix(List("A", "B", "C"), List("A", "B", "C")) shouldBe(List(), List())
    part_2("example2.txt") shouldBe 4
  }
}
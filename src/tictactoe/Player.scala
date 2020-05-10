package tictactoe

import tictactoe.Mark.Empty

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Random, Try}

sealed trait Player {
  def name: String
  def mark: Mark

  def next: Player

  def getSquare(grid: Grid): (Int, Int)
}

case object Human extends Player {
  override def name: String = "You"

  override val mark: Mark = Mark.X

  override val next: Player = Computer

  @tailrec
  override def getSquare(g: Grid): (Int, Int) = {
    val grid = g.value
    val (x, y) = readSquare(() => StdIn.readLine())
    if (x >= grid.length || y >= grid(0).length || grid(x)(y) != Empty) {
      println("Please enter a valid square")
      getSquare(g)
    } else (x, y)
  }

  @tailrec
  private def readSquare(readLine: () => String): (Int, Int) = {
    println(
      "You are player 'X'. Enter the square you want to play in the format: <row> <column>. Eg \"1 3\" is the top right square:"
    )
    val coord = readLine().split(" ")
    val x = safeOp(coord(0).toInt).map(_ - 1)
    val y = safeOp(coord(1).toInt).map(_ - 1)
    if (x.isDefined && y.isDefined) {
      (x.get, y.get)
    } else {
      println("Please enter the square using the correct format")
      readSquare(readLine)
    }
  }

  def safeOp[A](unsafe: => A): Option[A] = Try { unsafe }.toOption
}

case object Computer extends Player {
  override val name: String = "Computer"
  override val mark: Mark = Mark.O

  override val next: Player = Human

  @tailrec
  override def getSquare(grid: Grid): (Int, Int) = {
    val randomX = Random.nextInt(grid.rows())
    val randomY = Random.nextInt(grid.cols())
    if (grid.get(randomX, randomY).isEmpty) {
      (randomX, randomY)
    } else getSquare(grid)
  }
}

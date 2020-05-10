package tictactoe

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Random, Try}

trait Player {
  def name: String
  def sign: String

  def next: Player

  def getSquare(grid: Array[Array[String]]): (Int, Int)
}

case object Human extends Player {
  override def name: String = "You"

  override val sign: String = "X"

  override val next: Player = Computer

  @tailrec
  override def getSquare(grid: Array[Array[String]]): (Int, Int) = {
    val (x, y) = readSquare(() => StdIn.readLine())
    if (x >= grid.length || y >= grid(0).length || grid(x)(y).nonEmpty) {
      println("Please enter a valid square")
      getSquare(grid)
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
  override val sign: String = "O"

  override val next: Player = Human

  override def getSquare(grid: Array[Array[String]]): (Int, Int) = {
    val randomX = Random.nextInt(grid.length)
    val randomY = Random.nextInt(grid(0).length)
    if (grid(randomX)(randomY).isEmpty) {
      (randomX, randomY)
    } else getSquare(grid)
  }
}

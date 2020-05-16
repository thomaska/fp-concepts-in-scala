package tictactoe

import tictactoe.Mark.Empty

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Random, Try}

sealed trait Player {
  def name: String
  def mark: Mark

  def next: Player

  def getSquare(grid: Grid): IO[(Int, Int)]
}

case object Human extends Player {
  override def name: String = "You"

  override val mark: Mark = Mark.X

  override val next: Player = Computer

  override def getSquare(g: Grid): IO[(Int, Int)] = {
    val grid = g.value
    readSquare(Main.readLine()).flatMap {
      case (x, y) =>
        if (x >= grid.length || y >= grid(0).length || grid(x)(y) != Empty) {
          Main.putLine("Please enter a valid square").flatMap(_ => getSquare(g))
        } else IO(() => (x, y))
    }
  }

  private def readSquare(readLine: IO[String]): IO[(Int, Int)] = {
    for {
      _ <- Main.putLine(
        "You are player 'X'. Enter the square you want to play in the format: <row> <column>. Eg \"1 3\" is the top right square:"
      )
      coord <- Main.readLine().map(_.split(" "))
      x = safeOp(coord(0).toInt).map(_ - 1)
      y = safeOp(coord(1).toInt).map(_ - 1)
      res <- {
        if (x.isDefined && y.isDefined) {
          IO(() => (x.get, y.get))
        } else {
          println("Please enter the square using the correct format")
          readSquare(readLine)
        }
      }
    } yield res
  }

  def safeOp[A](unsafe: => A): Option[A] = Try { unsafe }.toOption
}

case object Computer extends Player {
  override val name: String = "Computer"
  override val mark: Mark = Mark.O

  override val next: Player = Human

  override def getSquare(grid: Grid): IO[(Int, Int)] = {
    val randomX = Random.nextInt(grid.rows())
    val randomY = Random.nextInt(grid.cols())
    if (grid.get(randomX, randomY).isEmpty) {
      IO(() => (randomX, randomY))
    } else getSquare(grid)
  }
}

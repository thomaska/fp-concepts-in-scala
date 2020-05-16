package tictactoe

import tictactoe.Mark.Empty

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.io.StdIn

/**
  * In this version:
  * - Created IO class for describing impure IO operations. More purity!
  */
object Main extends App {

  mainMenu(Grid(ArraySeq.fill(3, 3)(Empty))).run()

  def mainMenuAction(inputIO: IO[String]): IO[MainMenuAction] = {
    inputIO.map {
      case "Q" => QuitGame
      case "N" => NewGame
      case _   => InvalidAction
    }
  }

  def mainMenu(grid: Grid): IO[Unit] =
    for {
      _ <- putLine("Press N for new game, or Q to quit:")
      action <- mainMenuAction(readLine())
      _ <- action match {
        case QuitGame =>
          putLine("Exiting...Thank you for playing TicTacToe!")
        case NewGame =>
          for {
            _ <- printGrid(grid)
            _ <- playGame(grid, Human)
          } yield ()
        case InvalidAction =>
          for {
            _ <- putLine("Please enter a valid option.")
            _ <- mainMenu(grid)
          } yield ()
      }
    } yield ()

  def playGame(grid: Grid, currentPlayer: Player): IO[Unit] = {
    for {
      turn <- gameTurn(grid, currentPlayer)
      (updatedGrid, gameState) = turn
      _ <- printGrid(updatedGrid)
      _ <- gameState match {
        case Draw => putLine("The game ended in a draw!")
        case Win(p) => {
          p match {
            case Human    => putLine("Congratulations!! You win!")
            case Computer => putLine("Computer wins!")
          }
        }
        case OnGoing(nextPlayer) => playGame(updatedGrid, nextPlayer)
      }
    } yield ()
  }

  private def gameTurn(grid: Grid,
                       currentPlayer: Player): IO[(Grid, GameResult)] = {
    for {
      t <- currentPlayer.getSquare(grid)
      (x, y) = t
      updatedGrid = grid.set(x, y, currentPlayer.mark)
      _ <- putLine(s"${currentPlayer.name} played (${x + 1}, ${y + 1}) ")
    } yield {
      if (playerWon(currentPlayer.mark, updatedGrid))
        (updatedGrid, Win(currentPlayer))
      else if (checkDraw(updatedGrid))
        (updatedGrid, Draw)
      else (updatedGrid, OnGoing(currentPlayer.next))
    }
  }

  def checkRange(range: List[(Int, Int)], player: Mark, grid: Grid): Boolean = {
    val lines: List[List[(Int, Int)]] = range.grouped(3).toList
    val res = for {
      line <- lines
      lineRes = line
        .map { case (x, y) => grid.get(x, y) == player }
        .reduce(_ && _)
    } yield lineRes
    res.reduce(_ || _)
  }

  def putLine(s: String): IO[Unit] = {
    IO(() => println(s))
  }

  def readLine(): IO[String] = {
    IO(() => StdIn.readLine())
  }
  
  def rowRange = (0 to 8).map(_ / 3)
  def colRange = (0 to 8).map(_ % 3)

  def playerWon(player: Mark, grid: Grid): Boolean = {
    def rowsCheck = checkRange(rowRange.zip(colRange).toList, player, grid)
    def colsCheck = checkRange(colRange.zip(rowRange).toList, player, grid)
    def diagonalCheck = checkRange((0 to 2).zipWithIndex.toList, player, grid)
    def otherDiagonalCheck =
      checkRange((0 to 2).map(i => (2 - i, i)).toList, player, grid)
    rowsCheck || colsCheck || diagonalCheck || otherDiagonalCheck
  }

  def checkDraw(g: Grid): Boolean = {
    rowRange
      .zip(colRange)
      .toList
      .map { case (x, y) => !g.get(x, y).isEmpty }
      .reduce(_ && _)
  }

  def printGrid(g: Grid): IO[Unit] = {
    val linesToPrint = (0 to 2).map { i =>
      for {
        _ <- if (i != 0) {
          putLine("------------------------")
        } else IO.create(())

        _ <- putLine(
          s"\t${g.get(i, 0)}\t|\t${g.get(i, 1)}\t|\t${g.get(i, 2)}"
        )
      } yield ()
    } :+ putLine("\n")
    linesToPrint.reduce(
      (a, b) =>
        IO(() => {
          a.run()
          b.run()
          ()
        })
    )
  }
}

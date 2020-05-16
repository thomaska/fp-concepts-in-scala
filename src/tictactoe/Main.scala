package tictactoe

import tictactoe.Mark.Empty

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.io.StdIn

/**
  * In this version:
  * - Refactored playerWon and draw functions
  */
object Main extends App {

  mainMenu(Grid(ArraySeq.fill(3, 3)(Empty)))

  def mainMenuAction(input: () => String): MainMenuAction = {
    input() match {
      case "Q" => QuitGame
      case "N" => NewGame
      case _   => InvalidAction
    }
  }

  @tailrec
  def mainMenu(grid: Grid): Unit = {
    println("Press N for new game, or Q to quit:")
    val action = mainMenuAction(() => StdIn.readLine())
    action match {
      case QuitGame =>
        println("Exiting...Thank you for playing TicTacToe!")
      case NewGame =>
        printGrid(grid)
        playGame(grid, Human)
      case InvalidAction => {
        println("Please enter a valid option.")
        mainMenu(grid)
      }
    }
  }

  def playGame(grid: Grid, currentPlayer: Player): Unit = {

    val (updatedGrid, gameState) = gameTurn(grid, currentPlayer)
    printGrid(updatedGrid)
    gameState match {
      case Draw => println("The game ended in a draw!")
      case Win(p) => {
        p match {
          case Human    => println("Congratulations!! You win!")
          case Computer => println("Computer wins!")
        }
      }
      case OnGoing(nextPlayer) => playGame(updatedGrid, nextPlayer)
    }
  }

  private def gameTurn(grid: Grid,
                       currentPlayer: Player): (Grid, GameResult) = {
    val (x, y) = currentPlayer.getSquare(grid)
    val updatedGrid = grid.set(x, y, currentPlayer.mark)
    println(s"${currentPlayer.name} played (${x + 1}, ${y + 1})")
    if (playerWon(currentPlayer.mark, updatedGrid))
      (updatedGrid, Win(currentPlayer))
    else if (checkDraw(grid))
    (updatedGrid, Draw)
    else (updatedGrid, OnGoing(currentPlayer.next))
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

  def printGrid(g: Grid): Unit = {
    for (i <- 0 to 2) {
      if (i != 0) {
        println("------------------------")
      }
      println(s"\t${g.get(i, 0)}\t|\t${g.get(i, 1)}\t|\t${g.get(i, 2)}")
    }
    println("\n")
  }
}

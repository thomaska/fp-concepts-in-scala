package tictactoe

import tictactoe.Mark.Empty

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.io.StdIn

/**
  * In this version:
  * - Even more types!
  * - Made gameTurn pure
  */
object Main extends App {
  private val grid: Grid = Grid(ArraySeq.fill(3, 3)(Empty))

  mainMenu(grid)

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
    if (checkDraw(grid))
      (updatedGrid, Draw)
    else if (playerWon(currentPlayer.mark, updatedGrid))
      (updatedGrid, Win(currentPlayer))
    else (updatedGrid, OnGoing(currentPlayer.next))
  }

  def playerWon(player: Mark, grid: Grid): Boolean = {
    var allTheSame = true
    for (i <- 0 to 2) {
      allTheSame = true
      for (j <- 0 to 2) {
        if (grid.get(i, j) != player)
          allTheSame = false
      }
      if (allTheSame)
        return true
    }
    for (i <- 0 to 2) {
      allTheSame = true
      for (j <- 0 to 2) {
        if (grid.get(j, i) != player)
          allTheSame = false
      }
      if (allTheSame)
        return true
    }
    allTheSame = true
    for (i <- 0 to 2) {
      if (grid.get(i, i) != player)
        allTheSame = false
    }
    if (allTheSame)
      return true
    allTheSame = true
    for (i <- 0 to 2) {
      if (grid.get(2 - i, i) != player)
        allTheSame = false
    }
    allTheSame
  }

  def checkDraw(g: Grid): Boolean = {
    var thereIsEmptySquare = false
    for (i <- 0 to 2) {
      for (j <- 0 to 2) {
        if (g.get(i, j).isEmpty) {
          thereIsEmptySquare = true
        }
      }
    }
    !thereIsEmptySquare
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

package tictactoe

import tictactoe.Main.{grid, playerWon}

import scala.annotation.tailrec
import scala.io.StdIn

/**
  * In this version:
  * - Make better use of types
  * - Separate creating an action and handling it
  */
object Main extends App {
  val grid: Array[Array[String]] = Array.fill(3, 3)("")

  mainMenu(grid)

  def mainMenuAction(input: () => String): MainMenuAction = {
    input() match {
      case "Q" => QuitGame
      case "N" => NewGame
      case _   => InvalidAction
    }
  }

  @tailrec
  def mainMenu(grid: Array[Array[String]]): Unit = {
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

  def playGame(grid: Array[Array[String]], currentPlayer: Player): Unit = {

    val gameState = gameTurn(grid, currentPlayer)
    printGrid(grid)
    gameState match {
      case Draw => println("The game ended in a draw!")
      case Win(p) => {
        p match {
          case Human    => println("Congratulations!! You win!")
          case Computer => println("Computer wins!")
        }
      }
      case OnGoing(nextPlayer) => playGame(grid, nextPlayer)
    }
  }

  private def gameTurn(grid: Array[Array[String]],
                       currentPlayer: Player): GameResult = {
    val (x, y) = currentPlayer.getSquare(grid)
    grid(x)(y) = currentPlayer.sign
    println(s"${currentPlayer.name} played (${x + 1}, ${y + 1})")
    if (checkDraw(grid))
      Draw
    else if (playerWon(currentPlayer.sign, grid))
      Win(currentPlayer)
    else OnGoing(currentPlayer.next)
  }

  def playerWon(player: String, grid: Array[Array[String]]): Boolean = {
    var allTheSame = true
    for (i <- 0 to 2) {
      allTheSame = true
      for (j <- 0 to 2) {
        if (grid(i)(j) != player)
          allTheSame = false
      }
      if (allTheSame)
        return true
    }

    for (i <- 0 to 2) {
      allTheSame = true
      for (j <- 0 to 2) {
        if (grid(j)(i) != player)
          allTheSame = false
      }
      if (allTheSame)
        return true
    }

    allTheSame = true
    for (i <- 0 to 2) {
      if (grid(i)(i) != player)
        allTheSame = false
    }
    if (allTheSame)
      return true
    allTheSame = true
    for (i <- 0 to 2) {
      if (grid(2 - i)(i) != player)
        allTheSame = false
    }
    allTheSame
  }

  def checkDraw(grid: Array[Array[String]]): Boolean = {
    var thereIsEmptySquare = false
    for (i <- 0 to 2) {
      for (j <- 0 to 2) {
        if (grid(i)(j).isEmpty) {
          thereIsEmptySquare = true
        }
      }
    }
    !thereIsEmptySquare
  }

  def printGrid(grid: Array[Array[String]]): Unit = {
    for (i <- 0 to 2) {
      if (i != 0) {
        println("------------------------")
      }
      println(s"\t${grid(i)(0)}\t|\t${grid(i)(1)}\t|\t${grid(i)(2)}")
    }
    println("\n")
  }
}

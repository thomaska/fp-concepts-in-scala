package tictactoe

import tictactoe.Main.playerWon

import scala.annotation.tailrec
import scala.io.StdIn

/**
  * In this version:
  * - Better separation of different functionalities, more testable code
  * - Got rid of all the while loops
  * - Still sometimes the computer takes quite some time to play
  * - Would be good to make better use of types
  */
object Main extends App {
  val grid: Array[Array[String]] = Array.fill(3, 3)("")

  mainMenu(grid)
  @tailrec
  def mainMenu(grid: Array[Array[String]]): Unit = {
    println("Press N for new game, or Q to quit:")
    val input = StdIn.readLine
    input match {
      case "Q" =>
        println("Exiting...Thank you for playing TicTacToe!")
      case "N" =>
        printGrid(grid)
        playGame(grid, Human)
      case _ => {
        println("Please enter a valid option.")
        mainMenu(grid)
      }
    }
  }

  def playGame(grid: Array[Array[String]], currentPlayer: Player): Unit = {

    val (x, y) = currentPlayer.getSquare(grid)
    grid(x)(y) = currentPlayer.sign
    println(s"${currentPlayer.name} played (${x + 1}, ${y + 1})")
    printGrid(grid)
    if (checkDraw(grid))
      println("The game ended in a draw!")
    else if (playerWon(currentPlayer.sign, grid)) {
      currentPlayer match {
        case Human    => println("Congratulations!! You win!")
        case Computer => println("Computer wins!")
      }
    } else {
      playGame(grid, currentPlayer.next)
    }
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

package tictactoe

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Random, Try}

/**
  * Symptoms:
  * - Made functions total
  * - Sometimes the computer takes quite some time to play
  * - No testing
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
        playGame(grid)
      case _ => {
        println("Please enter a valid option.")
        mainMenu(grid)
      }
    }
  }

  def playGame(grid: Array[Array[String]]) = {
    var somebodyWon = false
    var draw = false

    while (!somebodyWon && !draw) {
      val (x, y) = readSquare(grid, () => StdIn.readLine())

      grid(x)(y) = "X"
      printGrid(grid)

      if (checkDraw(grid)) {
        println("The game ended in a draw!")
        draw = true
      }

      if (playerWon("X", grid)) {
        println("Congratulations!! You win!")
        somebodyWon = true
      }

      if (!somebodyWon && !draw) {
        println("Computer plays:")
        var computerIsPlaying = true
        while (computerIsPlaying) {
          val randomX = Random.nextInt(3)
          val randomY = Random.nextInt(3)
          if (grid(randomX)(randomY).isEmpty) {
            grid(randomX)(randomY) = "O"
            printGrid(grid)
            computerIsPlaying = false
          }
        }
        if (checkDraw(grid)) {
          println("The game ended in a draw!")
          draw = true
        }

        if (playerWon("O", grid)) {
          println("Computer wins!")
          somebodyWon = true
        }
      }
    }
  }

  @tailrec
  private def readSquare(grid: Array[Array[String]],
                         readLine: () => String): (Int, Int) = {
    println(
      "You are player 'X'. Enter the square you want to play in the format: <row> <column>. Eg \"1 3\" is the top right square:"
    )
    val coord = readLine().split(" ")
    val x = safeOp(coord(0).toInt).map(_ - 1)
    val y = safeOp(coord(1).toInt).map(_ - 1)
    if (x.isDefined && y.isDefined && x.get < grid.length && y.get < grid(0).length) {
      (x.get, y.get)
    } else {
      println("Please enter the square using the correct format")
      readSquare(grid, readLine)
    }
  }

  def safeOp[A](unsafe: => A): Option[A] = Try { unsafe }.toOption

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

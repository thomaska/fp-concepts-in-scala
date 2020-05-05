package tictactoe

import scala.io.StdIn
import scala.util.{Random, Try}

/**
  * Symptoms:
  * - Made functions total
  * - Sometimes the computer takes quite some time to play
  * - No testing
  */
object Main extends App {
  var grid = Array.fill(3, 3)("")
  var loop = true

  while (loop) {
    println("Press N for new game, or Q to quit:")
    val input = StdIn.readLine
    input match {
      case "Q" =>
        System.exit(1)
      case "N" =>
        printGrid
        loop = false
      case _ => println("Please enter a valid option.")
    }
  }

  var somebodyWon = false
  var draw = false

  while (!somebodyWon && !draw) {
    val (x ,y) = readSquare

    grid(x)(y) = "X"
    printGrid

    if (checkDraw()) {
      println("The game ended in a draw!")
      draw = true
    }

    if (playerWon("X")) {
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
          printGrid
          computerIsPlaying = false
        }
      }
      if (checkDraw()) {
        println("The game ended in a draw!")
        draw = true
      }

      if (playerWon("O")) {
        println("Computer wins!")
        somebodyWon = true
      }
    }
  }

  private def readSquare:(Int, Int) = {
    var invalidIntput = true
    var x, y: Option[Int] = None
    while (invalidIntput) {
      println(
        "You are player 'X'. Enter the square you want to play in the format: <row> <column>. Eg \"1 3\" is the top right square:"
      )
      val coord = StdIn.readLine().split(" ")
      x = safeInt(coord(0).toInt).map(_ - 1)
      y = safeInt(coord(1).toInt).map(_ - 1)
      if(x.isDefined && y.isDefined && x.get < grid.length && y.get < grid(0).length)
        invalidIntput = false
      else {
        println("Please enter the square using the correct format")
      }
    }
    (x.get, y.get)
  }

  def safeInt(unsafeInt: => Int): Option[Int] = Try { unsafeInt }.toOption

  def playerWon(player: String): Boolean = {
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

  def checkDraw(): Boolean = {
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

  def printGrid: Unit = {
    for (i <- 0 to 2) {
      if (i != 0) {
        println("------------------------")
      }
      println(s"\t${grid(i)(0)}\t|\t${grid(i)(1)}\t|\t${grid(i)(2)}")
    }
    println("\n")
  }
}

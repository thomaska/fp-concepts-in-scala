package tictactoe

import scala.collection.immutable.ArraySeq

case class Grid(value: ArraySeq[ArraySeq[Mark]]) {

  def rows(): Int = value.length

  def cols(): Int = value(0).length

  def get(row: Int, col: Int): Mark = {
    value(row)(col)
  }

  def set(row: Int, col: Int, m: Mark): Grid = {
    val copy = ArraySeq.tabulate(3, 3) { (i, j) =>
      if (i == row && j == col)
        m
      else value(i)(j)
    }
    Grid(copy)
  }
}

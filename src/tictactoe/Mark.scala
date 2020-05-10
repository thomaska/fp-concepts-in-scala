package tictactoe

sealed trait Mark {
  def isEmpty = this == Mark.Empty
}

object Mark {

  case object X extends Mark {
    override def toString: String = "X"
  }

  case object O extends Mark {
    override def toString: String = "O"
  }

  case object Empty extends Mark {
    override def toString: String = ""
  }
}

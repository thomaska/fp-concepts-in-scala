package tictactoe

sealed trait MainMenuAction

case object QuitGame extends MainMenuAction
case object NewGame extends MainMenuAction
case object InvalidAction extends MainMenuAction

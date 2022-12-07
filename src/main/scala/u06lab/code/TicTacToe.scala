package u06lab.code

import u06lab.code.ConnectThree.*

object TicTacToe extends ConnectThree(boardSize = 3):
  override def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 to bound // for all columns X
      y <- 0 to bound // for all row Y
      if find(board, x, y).isEmpty //if the position is
    yield
      Disk(x, y, player) +: board

  println("Mosse possibili da board vuota")
  printBoards(placeAnyDisk(List(), Player.X))
  println("Mosse possibili da board con angolo occupato")
  printBoards(placeAnyDisk(List(Disk(0, 0, Player.O)), Player.X))
  println("Mosse possibili da board con più mosse")
  printBoards(placeAnyDisk(List(Disk(1,2,Player.O), Disk(2,1,Player.O), Disk(2,0,Player.O)), Player.X))

  val moves = 4
  println(s"ANY GAME con $moves mosse")
  computeAnyGame(Player.O, moves).foreach { g =>
    printBoards(g)
    println()
  }
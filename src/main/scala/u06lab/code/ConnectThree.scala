package u06lab.code

import java.util.OptionalInt

enum Player:
  case X, O
  def other: Player = this match
    case X => O
    case _ => X

case class ConnectThree(boardSize:Int = 4) extends App:
  val bound = boardSize-1

  case class Disk(x: Int, y: Int, player: Player)
  /**
   * Board:
   * y
   * |
   * 3| | | | |
   * 2| | | | |
   * 1| | | | |
   * 0| | | | |
   *   0 1 2 3 - x
   */
  type Board = Seq[Disk]
  type Game = Seq[Board]

  import Player.*

  def find(board: Board, x: Int, y: Int): Option[Player] = board.find(d => d.x==x && d.y==y).map(_.player)

  def firstAvailableRow(board: Board, row: Int): Option[Int] =
    for col <- 0 to bound do
      board.find(disk => disk.x==row && disk.y==col) match
        case None => return Some(col)
        case _ =>
    return None

  //place AnyDisk returns a sequence of possible boards for every column you can insert a disk
  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 to bound // for all columns x
      y = firstAvailableRow(board, x)
      if y.isDefined //y can be None if the col X is full
    yield
      Disk(x, y.get, player) +: board

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match
    case 0 => LazyList[Game](List(List())) //caso base, quando moves = 0 la lista di game è un game con una lista di 0 board
    case _ =>
      for {
        game <- computeAnyGame(player.other, moves - 1) //la lista di tutti i game, nel for acquista il valore di volta in volta di uno di essi
        lastBoard = game.head //prendo l'ultima board dal game di sequenze di board
        //devo tornare tutte le board possibili facendo una mossa
        nextBoardPossible <- placeAnyDisk(lastBoard, player) //lista di tutte le board possibili, nel for acquista il valore di una di esse
      } yield
        //qui ho una matrice di game uno a uno e per ognuno di board, con prossime mosse, una a una
        nextBoardPossible +: game //aggiungo di volta il volta la prossima board al game

  def printBoards(game: Seq[Board]): Unit =
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()

object ConnectThreeImpl extends ConnectThree:
  import Player.*
  // Exercise 1: implement find such that..
  println("EX 1: ")
  println(find(List(Disk(0, 0, X)), 0, 0)) // Some(X)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1)) // Some(O)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1)) // None

  // Exercise 2: implement firstAvailableRow such that..
  println("EX 2: ")
  println(firstAvailableRow(List(), 0)) // Some(0)
  println(firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None
  // Exercise 2: implement placeAnyDisk such that..
  printBoards(placeAnyDisk(List(), X))
  // .... .... .... ....
  // .... .... .... ....
  // .... .... .... ....
  // ...X ..X. .X.. X...
  printBoards(placeAnyDisk(List(Disk(0, 0, O)), X))
  // .... .... .... ....
  // .... .... .... ....
  // .... .... .... X...
  // O..X O.X. OX.. O...

  println("EX 3: ")
  // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  computeAnyGame(O, 4).foreach { g =>
    printBoards(g)
    println()
  }
  //  .... .... .... .... ...O
  //  .... .... .... ...X ...X
  //  .... .... ...O ...O ...O
  //  .... ...X ...X ...X ...X
  //
  //
  // .... .... .... .... O...
  // .... .... .... X... X...
  // .... .... O... O... O...
  // .... X... X... X... X...

  // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
  // see TicTacToe class
package u05lab.code

import u05lab.code.TicTacToe.{columns, markBoard, rows}

import scala.collection.mutable
import scala.util.Random

object TicTacToe extends App {
  sealed trait Player{
    def other: Player = this match {case X => O; case _ => X}
    override def toString: String = this match {case X => "X"; case _ => "O"}
  }
  case object X extends Player
  case object O extends Player

  final val rows = 3
  final val columns = 3

  case class Mark(x: Double, y: Double, player: Player)
  type Board = List[Mark]
  type Game = List[Board]

  var markBoard: mutable.MutableList[Mark] = new mutable.MutableList[Mark]()

  def find(board: Board, x: Double, y: Double): Option[Player] = board match {
    case Mark(`x`, `y`, p) :: _ => Some(p)
    case _ :: t => find(t, x, y)
    case _ => None
  }

  def placeAnyMark(board: Board, player: Player): Seq[Board] = {
    board match {
      case Mark(x, y, p) :: _ => markBoard += Mark(x, y, p)
      case _ :: t => placeAnyMark(board, player)
      case _ => markBoard += Mark(Random.nextInt(rows), Random.nextInt(columns), player)
    }
    Seq(markBoard.toList)
  }

//  def computeAnyGame(player: Player, moves: Int): Stream[Game] = ???

  def printBoards(game: Seq[Board]): Unit =
    for (y <- 0 to 2; board <- game.reverse; x <- 0 to 2) {
      print(find(board, x, y) map (_.toString) getOrElse ("."))
      if (x == 2) { print(" "); if (board == game.head) println()}
    }

  // Exercise 1: implement find such that..
  println(find(List(Mark(0,0,X)),0,0)) // Some(X)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),0,1)) // Some(O)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),1,1)) // None

  // Exercise 2: implement placeAnyMark such that..
  printBoards(placeAnyMark(List(),X))
  //... ... ..X ... ... .X. ... ... X..
  //... ..X ... ... .X. ... ... X.. ...
  //..X ... ... .X. ... ... X.. ... ...
  printBoards(placeAnyMark(List(Mark(0,0,O)),X))
  //O.. O.. O.X O.. O.. OX. O.. O..
  //... ..X ... ... .X. ... ... X..
  //..X ... ... .X. ... ... X.. ...

  // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
//  computeAnyGame(O, 4) foreach {g => printBoards(g); println()}
  //... X.. X.. X.. XO.
  //... ... O.. O.. O..
  //... ... ... X.. X..
  //              ... computes many such games (they should be 9*8*7*6 ~ 3000).. also, e.g.:
  //
  //... ... .O. XO. XOO
  //... ... ... ... ...
  //... .X. .X. .X. .X.

  // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
}
package ex4

import scala.util.Random

// Optional!
object ConnectThree extends App:
  val bound = 3
  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X

  case class Disk(x: Int, y: Int, player: Player)
  // Board:
  // y
  //
  // 3
  // 2
  // 1
  // 0
  //   0 1 2 3   x
  type Board = Seq[Disk]
  type Game = (boards: Seq[Board], won: Boolean)
  def emptyGame: Game = (Seq(Seq()), false)

  import Player.*

  def find(board: Board, x: Int, y: Int): Option[Player] =
    board.find(disk => disk.x == x && disk.y == y).map(_.player)

  def firstAvailableRow(board: Board, x: Int): Option[Int] =
    board
      .collect { case Disk(`x`, y, _) => y }
      .maxOption
      .map(_ + 1)
      .orElse(Some(0))
      .filter(_ <= bound)

  def computeAnyDisk(board: Board, player: Player): Seq[Disk] =
    for
      x <- 0 to bound
      y <- firstAvailableRow(board, x)
    yield Disk(x, y, player)

  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for disk <- computeAnyDisk(board, player)
    yield board :+ disk

  def lastPlayer(initialPlayer: Player, moves: Int): Player =
    if moves % 2 == 0 then initialPlayer.other else initialPlayer

  def computeAnyGame(initialPlayer: Player, moves: Int): LazyList[Game] =
    def evolveGame(game: Game, currentPlayer: Player): Seq[Game] =
      val lastBoard = game.boards.head
      if (game.won || hasSomeoneWon(lastBoard))
        Seq((game.boards, true))
      else
        for newBoard <- placeAnyDisk(lastBoard, currentPlayer)
        yield (newBoard +: game.boards, false)

    def step(currentPlayer: Player, remainingMoves: Int): LazyList[Game] =
      remainingMoves match
        case 0 => LazyList(emptyGame)
        case _ =>
          for
            game <- step(currentPlayer.other, remainingMoves - 1)
            newGame <- evolveGame(game, currentPlayer)
          yield newGame

    step(lastPlayer(initialPlayer, moves), moves)

  def hasSomeoneWon(board: Board): Boolean =
    Seq(X, O).exists(hasPlayerWon(board, _))

  def hasPlayerWon(board: Board, player: Player): Boolean =
    import math.abs
    val playerDisks = board.filter(_.player == player)

    def hasThreeAligned(disks: Seq[Disk]): Boolean =
      disks
        .combinations(3)
        .exists:
          _.sortBy(d => (d.x, d.y)) match
            case a :: b :: c :: _ =>
              val dx1 = b.x - a.x
              val dy1 = b.y - a.y
              val dx2 = c.x - b.x
              val dy2 = c.y - b.y
              dx1 == dx2 && dy1 == dy2 && (abs(dx1) == 1 || abs(dy1) == 1)

    hasThreeAligned(playerDisks)

  def printBoards(game: Seq[Board]): Unit =
    val winner = Seq(X, O)
      .find(hasPlayerWon(game.headOption.getOrElse(Seq()), _))
      .getOrElse("None")
    // if winner == "None" then return // DEBUG
    println(s"Game won by: $winner")
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()

  // Exercise 1: implement find such that...
  println("EX 1: ")
  println(find(List(Disk(0, 0, X)), 0, 0)) // Some(X)
  // Some(O)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1))
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1)) // None

  // Exercise 3: implement firstAvailableRow such that...
  println("EX 3: ")
  println(firstAvailableRow(List(), 0)) // Some(0)
  println(firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
  println(
    firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)
  ) // Some(3)
  println(
    firstAvailableRow(
      List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)),
      0
    )
  ) // None

  // Exercise 2: implement placeAnyDisk such that...
  println("EX 2: ")
  printBoards(placeAnyDisk(List(), X))
  // .... .... .... ....
  // .... .... .... ....
  // .... .... .... ....
  // ...X ..X. .X.. X...
  printBoards(placeAnyDisk(List(Disk(3, 0, O)), X))
  // .... .... .... ....
  // .... .... .... ....
  // ...X .... .... ....
  // ...O ..XO .X.O X..O

  // Exercise 4 (ADVANCED!): implement computeAnyGame such that...
  println("EX 4: ")
  val start = System.currentTimeMillis()
//  Uncomment to show results
//  computeAnyGame(O, 8).foreach { g =>
//    printBoards(g.boards)
//    println()
//  }
  computeAnyGame(O, 8).size
  println(s"Taken ${(System.currentTimeMillis() - start) / 1000.0} seconds")
  //  .... .... .... .... ...O
  //  .... .... .... ...X ...X
  //  .... .... ...O ...O ...O
  //  .... ...X ...X ...X ...X
  //
  // .... .... .... .... O...
  // .... .... .... X... X...
  // .... .... O... O... O...
  // .... X... X... X... X...

  // Exercise 6 (ADVANCED!) -- create an AI that plays...
  trait AI:
    def computeNewDisk(board: Board, player: Player): Disk

  object AI:
    def randomAI: AI = RandomAIImpl()

    private class RandomAIImpl extends AI:
      private val random = Random()

      override def computeNewDisk(board: Board, player: Player): Disk =
        val disks = computeAnyDisk(board, player)
        val index = random.nextInt(disks.size)
        disks(index)

  def simulateGame(initialPlayer: Player, moves: Int)(using ai: AI): Game =
    def step(currentPlayer: Player, remainingMoves: Int): Game =
      remainingMoves match
        case 0 => emptyGame
        case _ =>
          val previousBoards =
            step(currentPlayer.other, remainingMoves - 1).boards
          val lastBoard = previousBoards.head
          val newDisk = ai.computeNewDisk(lastBoard, currentPlayer)
          val newBoard = lastBoard :+ newDisk
          (newBoard +: previousBoards, false)

    step(lastPlayer(initialPlayer, moves), moves)

  given AI = AI.randomAI
  val game = simulateGame(X, 6)
  printBoards(game.boards)

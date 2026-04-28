package ex3
import scala.math.ceil

object Solitaire:
  type Position = (x: Int, y: Int)
  type Dimension = (h: Int, w: Int)
  type Solution = Seq[Position]
  type SeqFactory = Solution => Seq[Solution]
  val dim: Dimension = (5, 5)
  given SeqFactory = LazyList(_)

  def placeMarks(n: Int = dim.w * dim.h)(using
      factory: SeqFactory
  ): Seq[Solution] = n match
    case 0 => factory(List())
    case 1 => factory(List((ceil(dim.h / 2.0).toInt, ceil(dim.w / 2.0).toInt)))
    case _ =>
      for
        marks <- placeMarks(n - 1)
        row <- 1 to dim.h
        col <- 1 to dim.w
        mark = (row, col)
        if !marks.contains(mark)
        if marks.isEmpty || isMarkValid(mark, marks.head)
      yield mark +: marks // Prepend is faster than append with List

  def isMarkValid(newMark: Position, lastMark: Position): Boolean =
    ((newMark.x - lastMark.x).abs == 3 && (newMark.y == lastMark.y))
      | ((newMark.x == lastMark.x) && (newMark.y - lastMark.y).abs == 3)
      | ((newMark.x - lastMark.x).abs == 2 && (newMark.y - lastMark.y).abs == 2)

  def render(
      si: (solution: Solution, index: Int),
      width: Int = dim.w,
      height: Int = dim.h
  ): Unit =
    println(); println(s"sol ${si.index}")
    val reversed = si.solution.reverse
    val rows =
      for
        x <- 1 to height
        row =
          for
            y <- 1 to width
            number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    println(rows.mkString("\n"))

  @main def run(): Unit =
    val start = System.currentTimeMillis()
    placeMarks().zipWithIndex foreach (render(_))
    print(s"Taken ${(System.currentTimeMillis() - start) / 1000.0} seconds")

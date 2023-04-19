package u06lab.code

import scala.annotation.tailrec

object Solitaire extends App:
  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  def placeMarks(width: Int, height: Int): Unit =
    val pos: Int = (width * height) / 2
    var solution: Seq[(Int, Int)] = Seq((pos - (((pos + 1) / width) * width), (pos + 1) / height))
    var board: Array[Int] = Array(0)
    for
      i <- 1 to ((width * height) - 1)
    do board = board :+ i

//    board.foreach(elem =>
//      if elem % 7 == 0 && elem != 0 then println()
//      print(elem + " ")
//    )

    def recursivePos(newPos: Int, numPlaced: Int, newBoard: Array[Int]): Unit =
      if numPlaced == 35 then println(render(solution = solution, width = width, height = height)); println()
      val possiblePos: Array[Int] = Array(
        newPos-(width*3), newPos-((width*2)+2), newPos-((width*2)-2), newPos-3, newPos+3, newPos+((width*2)-2), newPos+((width*2)+2), newPos+(width*3)
      )
      possiblePos.foreach(elem =>
        print(elem + " ")
      )
      println()

      possiblePos.foreach(calcPos =>
        if calcPos>=0 && calcPos<(width*height)-1 && numPlaced != 35 && newBoard(calcPos) == 0
        then
          solution = solution :+ ( calcPos - (((calcPos + 1) / width) * width), (calcPos + 1) / height )
          print(solution)
          newBoard(newPos) = numPlaced
          recursivePos(pos, numPlaced + 1, newBoard)
      )
    recursivePos(pos, 1, board)

//  println(render(solution = Seq((0, 0), (2, 1)), width = 3, height = 3))
  placeMarks(width = 7, height = 7)


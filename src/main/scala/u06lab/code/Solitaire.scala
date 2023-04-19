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
    val coordX: Int = width + 6
    val coordY: Int = height + 6
    val pos: Int = (coordX * coordY) / 2
    var sol: Seq[(Int, Int)] = Seq((pos - ((pos / coordX) * coordX) - 3, (pos / coordX) - 3))
    var board: Array[Int] = Array(-1)
    var count: Int = 0
    for
      _ <- 0 until ((width + 6) * (height + 6)) - 1
    do board = board :+ -1

    for
      i <- ((width + 6) * 3) + 3 until (((width + 6) * 3) + (3 + width))
      j <- 1 to height - 1
    do
      board(i) = 0
      board(i+((width + 6) * j)) = 0
    board(pos) = 1

//    var lol: Int = 1
//    board.foreach(elem =>
//      print(elem + " ")
//      if lol % 13 == 0 && lol != 0 then println()
//      lol = lol + 1
//    )
    def recursivePos(newPos: Int, newBoard: Array[Int], oldSol: Seq[(Int, Int)]): Unit =
      var newSol = oldSol
//      println("oldSol " + oldSol)
//      println("newSol " + newSol)
//      println()
      val possiblePos: Array[Int] = Array(
        newPos - (coordX * 3), (newPos - (coordX * 2)) + 2, (newPos - (coordX * 2)) - 2, newPos - 3, newPos + 3, (newPos + (coordX * 2)) - 2, (newPos + (coordX * 2)) + 2, newPos + (coordX * 3)
      )

      for
        calcPos <- possiblePos
      do
        if newBoard(calcPos) != -1 && newBoard(calcPos) == 0 then
          //          println("elem " + calcPos)
          newBoard(calcPos) = newBoard(newPos) + 1
          var x: Int = calcPos - ((calcPos / coordX) * coordX) - 3
          var y: Int = (calcPos / coordX) - 3
          if x < 0 then x = 0
          if y < 0 then y = 0
          newSol = newSol :+ (x, y)

          if newBoard(calcPos) < 2 then

          //            println("OldPos " + newPos + " - NewPos " + calcPos)
          //            print("X " + x + " ");
          //            println("Y " + y)
          //            println()
          //            println(newSol)
            recursivePos(calcPos, newBoard, newSol)
          else
//            count = count + 1
            println(render(solution = newSol.reverse, width = width, height = height))
            println()
          newBoard(calcPos) = 0
          newSol = newSol.dropRight(1)
    //          print("lol ")
    //          println(newSol)

    recursivePos(pos, board, sol)
//    println("Soluzioni sono " + count)
    @tailrec
    def lol(ciao: Int, ciao2: Int): Unit =
      print(ciao + " " + ciao2)
      lol(for i <- 1 to 10 yield i, for j <- 1 to 10 yield j)


//  println(render(solution = Seq((0, 0), (2, 1)), width = 3, height = 3))
//  placeMarks(width = 7, height = 7)
    lol(0, 0)

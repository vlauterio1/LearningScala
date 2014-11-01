package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def createPascalTriangle(rows: Int): Unit = {
      def processRow(row: Int): Unit = {
        if (row < rows) {
          values(row) = Array.ofDim[Int](row + 1)

          processCol(row, 0)
          processRow(row + 1)
        }
      }

      def processCol(row: Int, col: Int): Unit = {
        if (col < row + 1) {
          values(row)(col) = setValue(row, col)

          processCol(row, col + 1)
        }
      }

      def setValue(row: Int, col: Int): Int = {
        if (row > 0 && col < row)
          values(row - 1)(col) + (
            if (col == 0) 0
            else values(row -1)(col -1))
        else 1
      }

      processRow(0)
    }

    if (c > r)
      throw new Exception("Invalid position")

    lazy val values = Array.ofDim[Array[Int]](r + 1)

    createPascalTriangle(r + 1)

    values(r)(c)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    var cnt = 0

    def balanceInner(chars: List[Char]): Boolean = {
      if (!chars.isEmpty) {
        val c = chars.head

        c match {
          case '(' => cnt += 1
          case ')' => {
            cnt -= 1

            if (cnt < 0) return false
          }
          case _ => ()
        }

        balanceInner(chars.tail)
      }

      cnt == 0
    }

    balanceInner(chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    var cnt: Int = 0

    def countChangeInner(m: Int) {
//      coins.foreach {
//        case `money` => cnt += 1
//        case t if t > m => countChangeInner(money - t)
//      }

      coins.foreach({c =>
        c match {
          case `money` => cnt += 1
          case t if t > m => countChangeInner(money - t)
          case _ =>
        }
      })
    }

    cnt
  }
}

class PascalTriangle(rows: Int) {
	private def create() = {
		def setRow(row: Int): Unit = {
			if (row < rows) { 
				values(row) = Array.ofDim[Int](row + 1)
				
				setCol(row, 0)
				setRow(row + 1)
			}
		}

		def setCol(row: Int, col: Int): Unit = {
			if (col < row + 1) {
				values(row)(col) = getValue(row, col)

				setCol(row, col + 1)
			}
		}

		def getValue(row: Int, col: Int): Int = {
			if (row > 0 && col < row)
				values(row - 1)(col) + (
					if (col == 0) 0
					else values(row -1)(col -1))
			else 1
		}

		setRow(0)
	}

	override def toString() : String = {
		values.deep.mkString("\n")
	}

	var this.rows = rows
	var values = Array.ofDim[Array[Int]](rows)

	create()
}

object Main {
	def main(args: Array[String]): Unit = {
		val p = new PascalTriangle(args(0).toInt)

		println(p.toString)
	}
}
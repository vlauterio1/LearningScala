object Reverse {
	def main(args: Array[String]) {
		println(reverse("I'm a string "))
	}

	def reverse(s: String): String = {
		val len = s.length

		if (len == 0) return ""

		reverse(s.tail) + s.head
	}
}
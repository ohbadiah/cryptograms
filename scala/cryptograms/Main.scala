package cryptograms

import scala.util.Random

object Main {
	def main(args: Array[String]): Unit = {
	  val alph = ('a' to 'z') toSet
	  val enc = new Encoder(alph, new Random)
	  println(enc.createCipher.toString)
	}
}
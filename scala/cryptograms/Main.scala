package cryptograms

import scala.util.Random
import scala.io.Source

object Main {
	def main(args: Array[String]): Unit = {
	  if (args.length == 0) println("No input file given.")
	  else {
	    val alph = ('a' to 'z') toSet
	    val enc = new Encoder(alph, new Random)
	    val ciph = enc.createCipher
	    val encipherer = enc.enCipher(ciph)_
	    val file = Source.fromFile(args(0))
	    
	    for (line <- file.getLines())
	      println(encipherer(line))
	    file.close
	  }
	  
	}
}
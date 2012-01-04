package cryptograms
import scala.util.Random
import scala.collection.immutable.Map
import scala.collection.immutable.Set

class Encoder(chars: Set[Char], gen: Random) {
	private val alphabet = ('a' to 'z').toSet
	private val alphabetUL = ('A' to 'Z').toSet & alphabet
	
	def createCipher: Map[Char, Char] = {
	  val low = chars map(c => c toString() toLowerCase() charAt(0))
	  val res = aux(low, Map.empty)
	  res match {
	    case None    => return createCipher
	    case Some(m) => return m 
	  }
	}
	
	private def aux(chars: Set[Char], m: Map[Char, Char]): Option[Map[Char, Char]] = chars.toList match {
	  case Nil 	   => Some(m)
	  case c :: cs => 
	    val avail = (alphabet &~ (m.values.toSet & Set(c))).toList
	  	val nc = {n: Int => avail apply gen.nextInt(n)}
	  	if (0 == avail.length)
	  	  None
	  	else 
	  	  aux(cs.toSet, (m + (c -> nc(avail.length))))
	}
	
	def enCipher(ciph: Map[Char, Char])(text: String): String = {
	  return text.toList map(c => ciph.getOrElse(c, c)) toString
	}
	
}
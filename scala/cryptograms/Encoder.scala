package cryptograms
import scala.util.Random
import scala.collection.immutable.Map

class Encoder(toEncipher: List[Char], gen: Random) {
	val alphabet = 'a' to 'z' toList
	val alphabetUL = ('A' to 'Z').toList ::: alphabet
	
	def createCipher(chars: List[Char], gen: Random): Map[Char, Char] = {
	  val low = chars map(c => c toString() toLowerCase() charAt(0)) distinct
	  val res = aux(low, Map.empty, gen)
	  res match {
	    case None    => return createCipher(low, new Random)
	    case Some(m) => return m 
	  }
	}
	
	private def aux(cs: List[Char], m: Map[Char, Char], gen: Random): Option[Map[Char, Char]] = cs match {
	  case Nil 	   => Some(m)
	  case c :: cs => 
	    val avail = alphabet.toSet.diff(m.values.toSet).toList.filter(cp => c != cp)
	 //   println("map is " + m.toString)
	  	val nc = {n: Int => avail apply gen.nextInt(n)}
	  	if (0 == avail.length)
	  	  None
	  	else 
	  	  aux(cs, (m + (c -> nc(avail.length))), gen)
	}
	
}
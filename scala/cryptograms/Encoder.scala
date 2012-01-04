package cryptograms
import scala.util.Random
import scala.collection.immutable.Map

class Encoder(chars: List[Char], gen: Random) {
	private val alphabet = 'a' to 'z' toList
	private val alphabetUL = ('A' to 'Z').toList ::: alphabet
	
	def createCipher: Map[Char, Char] = {
	  val low = chars map(c => c toString() toLowerCase() charAt(0)) distinct
	  val res = aux(low, Map.empty)
	  res match {
	    case None    => return createCipher
	    case Some(m) => return m 
	  }
	}
	
	private def aux(cs: List[Char], m: Map[Char, Char]): Option[Map[Char, Char]] = cs match {
	  case Nil 	   => Some(m)
	  case c :: cs => 
	    val avail = alphabet.toSet.diff(m.values.toSet).toList.filter(cp => c != cp)
	  	val nc = {n: Int => avail apply gen.nextInt(n)}
	  	if (0 == avail.length)
	  	  None
	  	else 
	  	  aux(cs, (m + (c -> nc(avail.length))))
	}
	
}
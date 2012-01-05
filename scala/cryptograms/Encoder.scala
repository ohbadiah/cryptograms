package cryptograms
import scala.util.Random
import scala.collection.immutable.Map
import scala.collection.immutable.Set

class Encoder(chars: Set[Char], gen: Random) {
	private val alphabet = ('a' to 'z').toSet
	private val alphabetUL = ('A' to 'Z').toSet & alphabet
	
	def toLower(c: Char): Char = {return c toString() toLowerCase() charAt(0)}
	def toUpper(c: Char): Char = {return c toString() toUpperCase() charAt(0)}
	
	def createCipher: Map[Char, Char] = {
	  val low = chars map(toLower)
	  val res = aux(low, Map.empty)
	  
	  res match {
	    case None    => return createCipher
	    case Some(m) => return m.foldRight(Map.empty[Char, Char])(
	        (t: (Char, Char), m: Map[Char, Char]) => t match {
	          case s@(k, v) => m + s + ((toUpper(k), toUpper(v)))
	        })
	  }
	}
	
	private def aux(chars: Set[Char], m: Map[Char, Char]): Option[Map[Char, Char]] = chars.toList match {
	  case Nil 	   => Some(m)
	  case c :: cs => 
	    val avail = (alphabet &~ (m.values.toSet ++ Set(c))).toList
	  	val nc = {n: Int => avail(gen.nextInt(n))}
	  	if (0 == avail.length)
	  	  None
	  	else 
	  	  aux(cs.toSet, (m + (c -> nc(avail.length))))
	}
	
	def enCipher(ciph: Map[Char, Char])(text: String): String = {
	  val lst = text.toList map(c => ciph.getOrElse(c, c))
	  return lst.foldRight("")((ch: Char, str: String) => ch + str)
	}
	
}
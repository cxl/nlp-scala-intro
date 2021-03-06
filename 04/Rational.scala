// When using this file interactively, do ":paste 02-Rational-2.scala"
// because it needs to load class and object definition at the same time.
// Alternatively, use sbt or the scala compiler
//

class Rational(n: Int, d: Int) {

  require(d != 0)

  private val g = Rational.gcd(n.abs, d.abs)
  val numer = n / g
  val denom = d / g

  def this(n: Int) = this(n, 1)

  def + (that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def + (i: Int): Rational =
    new Rational(numer + i * denom, denom)

  def - (that: Rational): Rational =
    new Rational(
      numer * that.denom - that.numer * denom,
      denom * that.denom
    )

  def - (i: Int): Rational =
    new Rational(numer - i * denom, denom)


  override def toString = numer +"/"+ denom

}

// Companion object for "static" private method gcd
object Rational {

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

}


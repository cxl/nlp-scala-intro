class OrderedRational(n: Int, m: Int) extends Rational(n: Int, m:Int) with Ordered[Rational] {

def compare(that: Rational) =
  this.numer* that.denom- that.numer* this.denom

}
  

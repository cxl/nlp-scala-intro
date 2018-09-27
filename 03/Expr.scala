
case class Var(name: String) extends Expr
case class Num(num: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

abstract class Expr {

 // Evaluating an expression 
 def eval: Double = this match {
   case v:Var => 0    // Variables evaluate to 0
   case Num(x) => x
   case BinOp("+", e1, e2) => e1.eval + e2.eval
   case BinOp("*", e1, e2) => e1.eval * e2.eval
   case UnOp("-", e) => - e.eval
   }

 // Evaluate an expression with a map for the variables
 def eval(env: Map[String, Double]): Double = this match {
   case Var(v) => env(v)    // Look up variable name in Map
   case Num(x) => x
   case BinOp("+", e1, e2) => e1.eval(env) + e2.eval(env)
   case BinOp("*", e1, e2) => e2.eval(env) * e2.eval(env)
   case UnOp("-", e) => - e.eval(env)
 }

}

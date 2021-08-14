package Acorn

object TestAcorn extends App {
  var exp: Expression = Sum(Number(42), Product(Number(3.14), Number(2.71)))
  println("the value of " + exp + " = " + exp.execute)
  exp = Product(Number(2), Product(Number(3), Number(5)))
  println("the value of " + exp + " = " + exp.execute)
}

/**
   OUTPUT
  the value of (+ 42.0 (+ 3.14 2.71)) = 50.5094
  the value of (+ 2.0 (+ 3.0 5.0)) = 30.0

 */

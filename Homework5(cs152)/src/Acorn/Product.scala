package Acorn

class Product(private val operand1:Expression, private val operand2:Expression) extends Expression {

  override def execute: Double = operand1.execute * operand2.execute
  override def toString: String = "(+ " + operand1 + " " + operand2 + ")"
}

object Product{
   def apply(op1:Expression,op2:Expression) = {
       new Product(op1,op2)
   }
}

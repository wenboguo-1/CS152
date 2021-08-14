package Acorn

class Sum(private val operand1: Expression, private val operand2:Expression) extends Expression {
     override def execute = operand1.execute + operand2.execute
     override def toString = "(+ " + operand1 + " " + operand2 + ")"
}

object Sum{
    def apply(op1:Expression,op2:Expression) = {
        new Sum(op1,op2)
    }
}
package Acorn

class Number(val num:Double) extends Expression {
   override def execute = num
   override def toString = ""+ num
}

object Number{
    def apply(num:Double) = {
      new Number(num)
    }
}

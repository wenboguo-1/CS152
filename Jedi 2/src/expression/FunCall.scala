package expression
import value._
import context.Environment
import context.alu;
case class FunCall(identifier: Identifier,list: List[Expression]) extends Expression {

  override def execute(environment: Environment): Value = {

      //Use tail recursion to handle the nested function call
      def helper(res:List[Expression], list:List[Expression]):Value = {
            if(list == Nil)
              alu.execute(identifier,res.asInstanceOf[List[Value]])
            else {
               helper(res :+ list.head.execute(environment).asInstanceOf[Expression],list.tail)
            }
      }
    helper(List(),list)
  }
}

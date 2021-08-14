package expression
import context.{Environment, TypeException}
import value._

case class Conjunction(list: List[Expression]) extends Expression {

  override def execute(environment: Environment): Value = {
      //Use tail recursion for the lazy evaluation
     def helper(result:Value, list: List[Expression]):Value ={
            if(list == Nil)
              result
            else if (!result.isInstanceOf[Boole])
              throw  new TypeException
            else if(result.asInstanceOf[Boole].toString.equals("false"))
               result
             else
               helper(list.head.execute(environment),list.tail)
         }
     helper(list.head.execute(environment),list.tail)
  }
}

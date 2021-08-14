package expression
import context._
import value.{Boole, Value}

case class Disjunction(val list: List[Expression]) extends Expression {

  override def execute(environment: Environment): Value = {
    def helper(result:Value, list: List[Expression]):Value ={
      if(list == Nil)
        result
      else if (!result.isInstanceOf[Boole])
        throw  new TypeException
      else if(result.asInstanceOf[Boole].toString.equals("true"))
        result
      else
        helper(list.head.execute(environment),list.tail)
    }
    helper(list.head.execute(environment),list.tail)
  }
}

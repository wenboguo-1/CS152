package expression
import context.Environment
import value._
case class Conditional(val cond:Expression,val cons:Expression, val alt: Expression = null) extends SpecialForm {

  override def execute(environment: Environment): Value = {
      if(cond.execute(environment).asInstanceOf[Boole].toString.equals("true"))
            cons.execute(environment)
         else if(cond.execute(environment).asInstanceOf[Boole].toString.equals("false") && alt != null)
           alt.execute(environment)
         else
           Notification("UNSPECIFIED")
  }

}

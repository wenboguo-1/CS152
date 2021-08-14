package expression
import value.Value
import context.Environment
import value.Notification
case class Declaration(val id:Expression,val value:Expression) extends SpecialForm {

    override def execute(environment: Environment): Value = {
         environment.put(id.asInstanceOf[Identifier],value.execute(environment))
         Notification("OK")
    }
}

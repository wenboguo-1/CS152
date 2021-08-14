package expression

import context.{Environment, UndefinedException}
import value.Value
import value.Notification
case class Identifier(val name:String) extends Expression {

  override def toString: String = name
  override def hashCode(): Int = this.toString.hashCode
  override def execute(environment: Environment): Value = {

    try {
      environment.apply(Identifier(name)) // get the value from the global env
    }catch  {
      case e: UndefinedException => {
         Notification("Undefined identifier: " + name)
      }
    }
  }
}

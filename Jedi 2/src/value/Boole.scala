package value
import context.Environment
import expression.Literal

case class Boole(val value:Boolean) extends Literal {
   def &&(other: Value):Boole = {
     other match {
       case x: Boole => Boole(x.value && this.value)
       case _ => throw new Exception("Bad value")
    }
  }
    def ||(other:Value):Boole = {
     other match {
       case x: Boole  =>  Boole(x.value || this.value)
       case _ => throw new Exception("Bad value")
     }
  }

  def unary_!():Boole = Boole(!this.value)
  override def equals(obj: Any): Boolean = {
     obj match {
       case x : Boole => x.isInstanceOf[Boole] && x.value == this.value
       case _ => false
     }
  }
  override def toString: String = this.value.toString
  override def hashCode(): Int = this.toString.hashCode
  override def execute(environment: Environment): Value = this
}

object Boole{

  def TRUE = Boole(true)
  def FALSE = Boole(false)
}

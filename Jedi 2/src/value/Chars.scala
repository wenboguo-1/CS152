package value

import context.{Environment, TypeException}
import expression.Literal

case class Chars(val value:String) extends Addable with Ordered[Value]{

  def size():Exact = Exact(this.value.length)
  override def +(other: Value): Addable = {
       other match {
         case x: Exact => Chars(this.value + x)
         case x: Inexact => Chars(this.value + x)
         case x: Chars => Chars(this.value + x);
         case _ => throw new TypeException("Addable operand required")
       }
  }
  def subChars(exc1:Exact,exc2:Exact):Chars = {
         Chars(value.substring(exc1.value,exc2.value))
  }

  override def compare(that: Value): Int = {
    that match {
      case x : Chars => this.value.compare(x.value)
      case _ => throw new TypeException("Not comparable")
    }
  }

  override def equals(obj: Any): Boolean = {
       obj match {
         case x : Chars => x.isInstanceOf[Chars] && x.value== this.value
         case _ => false
       }
  }

  override def execute(environment: Environment): Value = {
     this
  }
  override def toString: String = this.value
  override def hashCode(): Int = this.toString.hashCode

}


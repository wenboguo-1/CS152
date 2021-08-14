package value
import context.{Environment, IllegalValueException, TypeException}

case class Inexact(val value:Double) extends Numeric with Ordered[Value]{
  def +(other: Value): Addable =
    other match {
      case x: Exact => Inexact(this.value.toDouble + x.value.toDouble)
      case x: Inexact => Inexact(this.value + x.value)
      case x: Chars => Chars(this.value.toString + x.value)
      case _ =>  throw new TypeException("Must be addable")
    }

  def unary_-(): Numeric = Inexact(-this.value)

  override def compare(other: Value): Int =
    other match {
      case x: Exact => this.value.compare(x.value.toDouble)
      case x: Inexact => this.value.compare(x.value)
      case _ => throw new Exception
    }


  override def equals(other: Any): Boolean =
    other match {
      case x: Inexact => x.isInstanceOf[Inexact] && x.value == this.value
      case x: Exact => x.isInstanceOf[Exact] && x.value.toDouble == this.value
      case _ => false
    }

  override def -(other: Value): Numeric = {
        other match {
          case x:Inexact => Inexact(this.value - x.value)
          case x:Exact => Inexact(this.value.toDouble - x.value)
          case _ => throw new TypeException("Numeric operand required")
        }
  }
  override def *(other: Value): Numeric = {
    other match {
      case x:Inexact => Inexact(x.value * this.value)
      case x:Exact => Inexact(x.value.toDouble * this.value)
      case _ =>  throw new TypeException("Numeric operand required")
    }
  }
  override def /(other: Value): Numeric = {

      other match {
        case x:Inexact =>if(x.value== 0) throw new IllegalValueException("Divided by 0") else Inexact(this.value/x.value)
        case x:Exact => if(x.value == 0) throw new IllegalValueException("Divided by 0") else Inexact(this.value/x.value.toDouble)
        case _ => throw new TypeException("Numeric operand required")
      }
  }
  override def toString: String = this.value.toString
  override def hashCode(): Int = this.toString.hashCode

  override def execute(environment: Environment): Value = this
}


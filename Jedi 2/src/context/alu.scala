package context


import value._
import expression._

object alu extends App {


  def execute(opcode: Identifier, args: List[Value]):Value= opcode.name match {

    case "add" => add(args)            // n-ary
    case "mul" => mul(args)            // n-ary
    case "sub" => sub(args)            // n-ary
    case "div" => div(args)            // n-ary
    case "less" => less(args)          // binary
    case "equals" => same(args)        // binary
    case "more" => more(args)          // binary
    case "unequals" => unequals(args)  // binary
    case "not" => not(args)            // unary

    // TBC
  }

  private def add(args: List[Value]):Value= {

    def helper(result: Addable, unseen: List[Value]):Addable=
      if(unseen == Nil) result
      else helper(result + unseen.head, unseen.tail)

    if(args.size < 2) throw new TypeException("2 or more inputs required by +")
    args(0) match {
      case n: Addable => helper(args(0).asInstanceOf[Addable], args.tail )
      case _ => throw new TypeException("Inputs to + must be addable")
    }
  }


  private def less(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by <")
    if(!args(0).isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to < must be orderable")
    Boole(args(0).asInstanceOf[Ordered[Value]] < args(1))
  }

  private def mul(value: List[Value]):Value = {

       def helper(res:Numeric,unseen:List[Value]): Numeric ={
            if(unseen == Nil)
               res
            else
              helper(res * unseen.head,unseen.tail)
       }
    if(value.length < 2){
      throw new TypeException("At least two operands required")
    }
     value(0) match {
       case x:Numeric => helper(x.asInstanceOf[Numeric],value.tail)
       case _=> throw new TypeException("Inputs to * must be numeric")
     }
  }

  def div(value:List[Value]):Value = {
      def helper(res:Numeric,value:List[Value]):Numeric ={
          if(value == Nil)
            res
          else
            helper(res/value.head,value.tail)
      }
      if(value.length < 2)
        throw new TypeException("At least two operands required")
      else
        value(0) match {
          case x:Numeric => helper(x.asInstanceOf[Numeric],value.tail)
          case _=> throw new TypeException("Inputs to / must be numeric")
        }
  }

  def sub(value: List[Value]):Value={
      def helper(res:Numeric,value:List[Value]):Numeric = {
          if(value == Nil)
            res
          else
            helper(res - value.head,value.tail)
      }
     if(value.length < 2)
       throw new TypeException("At least two operands required ")
     else
       value(0) match {
         case x:Numeric => helper(x.asInstanceOf[Numeric],value.tail)
         case _=> throw new TypeException("Input to - must be numeric")
       }
  }

  def same(value:List[Value]):Boole = {
      if(value.length != 2)
        throw new TypeException("Only 2 inputs required by ==")
      else {
        Boole(value(0).asInstanceOf[Addable] == value(1))
      }
  }

  def more(value: List[Value]):Value= {
      if(value.length != 2)
         throw new TypeException("Only 2 inputs required by ==")
          if(!value(0).isInstanceOf[Ordered[Value]])throw new TypeException("Inputs to < must be orderable")
          Boole(value(0).asInstanceOf[Ordered[Value]] > value(1) )

  }
   def unequals(value: List[Value]):Value= {
       if(value.length != 2) throw  new TypeException("Only 2 inputs required by ==")

       Boole((value(0).asInstanceOf[Addable] != value(1)))
   }

   def not(value: List[Value]):Value = {
     if(value.length != 1) throw new TypeException("Only 1 inputs required by !")
     if(!value.head.isInstanceOf[Boole]) throw new TypeException("It must be boolean")
     !value.head.asInstanceOf[Boole]
   }



}

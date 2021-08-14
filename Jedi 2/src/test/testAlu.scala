package test
import context.alu
import expression.Identifier
import value.{Chars, Exact, Inexact}
import  value.Boole
object testAlu extends App{

  try {
    println(Exact(1)/Inexact(0.04))
    println(alu.execute(Identifier("add"), List(Exact(5), Exact(6), Inexact(7.0))))
    println(alu.execute(Identifier("add"), List(Chars("abc"), Exact(6), Inexact(7.0001))))
    println(alu.execute(Identifier("less"), List(Exact(3), Inexact(1.3))))
    println(alu.execute(Identifier("less"), List(Exact(1), Inexact(1.00001))))
    println(alu.execute(Identifier("more"), List(Chars("abc"), Chars("def"))))
    println(alu.execute(Identifier("equals"), List(Chars("abc"), Chars("abc"))))
    println(alu.execute(Identifier("more"), List(Exact(3), Inexact(1.3))))
    println(alu.execute(Identifier("more"), List(Exact(1), Inexact(1.0))))
    println(alu.execute(Identifier("more"), List(Inexact(1.0), Exact(1))))
    println(alu.execute(Identifier("unequals"), List(Exact(3), Inexact(1.3))))
    println(alu.execute(Identifier("div"), List(Exact(3),Exact(2))))
    println(alu.execute(Identifier("div"), List(Exact(3),Exact(1),Inexact(0.04))))
    println(alu.execute(Identifier("mul"), List(Exact(3),Exact(1),Inexact(0.04))))
    println(alu.execute(Identifier("div"), List(Exact(3),Exact(0))))

  } catch {
     case e: Exception => println(e)
  }

  try{
    println(alu.execute(Identifier("not"),List(Boole.TRUE)))
    println(alu.execute(Identifier("not"),List(Boole(false))))
    println(alu.execute(Identifier("less"), List(Exact(3), Chars("abc"))))

  }catch{
    case e:Exception => print(e)
  }
  /**
   *
   *
   * 25.0
   * 18.0
   * abc67.0
   * false
   * true
   * true
   * true
   * true
   * false
   * false
   * true
   * 1.5
   * 75.0
   * 0.12
   * context.IllegalValueException: Illegal Value
   * false
   * true
   * context.TypeException: Arguments must be comparable
   * Process finished with exit code 0
   */
}

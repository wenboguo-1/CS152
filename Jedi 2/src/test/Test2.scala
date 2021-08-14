package test
import expression.FunCall
import value.Chars
import context.alu
import expression.{Identifier, Literal}
import value.{Exact, Inexact, Value}
object Test2 extends App{
  println(alu.execute(Identifier("div"), List(Exact(3),Exact(1),Exact(1),Inexact(0.04))))
  println( FunCall(Identifier("add"),List(Exact(12),Chars("avc"))).execute(null))
  println(Chars("abc") + Inexact(0.34))
  println(Inexact(0.34) >Chars("123"))
  try{
    println(Inexact(1.0001) + Chars("123"))
    println(Chars("123") > Exact(456));
  }catch{
    case x: Exception => print(x)
  }
}

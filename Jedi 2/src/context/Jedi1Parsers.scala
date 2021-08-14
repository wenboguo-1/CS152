package context

import scala.util.parsing.combinator._
import expression._
import value._

/**
 *
 * disjunction reduces to conjunction reduces to equality ... reduces to term
 * if A reduces to B, then B will have higher precedence than A
 * Example: sum reduces to product, so a + b * c = a + (b * c)
 * Had to make some big corrections to numeral regex
 * This could probably have been a singleton
 */

class Jedi1Parsers extends RegexParsers {

  def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

  def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^ {
    case "def"~id~"="~exp => Declaration(id, exp)
  }

  def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^ {
    case "if"~"("~cond~")"~cons~None => Conditional(cond, cons)
    case "if"~"("~cond~")"~cons~Some("else"~alt) => Conditional(cond, cons, alt)
  }

  def  disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^ {
    case con ~ Nil => con
    case con ~ more => Disjunction(con::more)
  }
  def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality)  ^^ {
    case con ~ Nil =>  con
    case con ~ more => Conjunction(con::more)
  }

  // conjunction ::= equality ~ ("&&" ~ equality)*
  def  equality:Parser[Expression] = inequality ~ opt("==" ~ inequality) ^^ {
    case con ~ None => con
    case con ~Some("==" ~ more) => FunCall(Identifier("equals"),List(con,more))
  }

  // inequality ::= sum ~ (("<" | ">" | "!=") ~ sum)?
  def inequality:Parser[Expression] = sum ~ opt(("<" | ">" | "!=") ~ sum) ^^{
    case s ~ None => s
    case s ~ Some("<" ~ more)=> FunCall(Identifier("less"),List(s,more))
    case s ~ Some(">" ~ more) => FunCall(Identifier("more"),List(s,more))
    case s ~ Some("!=" ~ more) => FunCall(Identifier("unequals"),List(s,more))
  }


  // sum ::= product ~ ("+" | "-") ~ product)*
  def sum: Parser[Expression] = product ~ rep(("+"|"-") ~ product) ^^ {
    case p ~ Nil => p
    case p ~  more => parseSums(p, more)
  }

  // use tail recursion to imitate left reduce
  // parses a - b + c into add(sub(a, b), c)
  private def parseSums(result: Expression, unseen: List[String ~ Expression]): Expression = {

    def combiner(exp: Expression, next: String~Expression) =

      next match {
        case "+" ~ p => FunCall(Identifier("add"), List(exp, p))
        case "-" ~ p => FunCall(Identifier("sub"), List(exp, p))
      }
    if (unseen == Nil) {
      result

    } else parseSums(combiner(result, unseen.head), unseen.tail)
  }

  // product ::= term ~ (("*" | "/") ~ term)*
  def product:Parser[Expression] = term ~ rep(("*" | "/") ~ term) ^^ {
    case t ~ Nil => t
    case t ~ more => parseProduct(t,more)
  }

  def parseProduct(result:Expression,list:List[String ~ Expression]):Expression={

     def combine(res:Expression,next:String ~ Expression): Expression ={
         next match {
           case "*" ~ p => FunCall(Identifier("mul"),List(res,p))
           case "/" ~ p => FunCall(Identifier("div"),List(res,p))
         }
     }

    if(list == Nil) result else parseProduct(combine(result,list.head),list.tail)
  }

  def identifier:Parser[Identifier] = "[a-zA-Z][a-zA-Z0-9]*".r ^^ {
    case s => Identifier(s)
  }

  def term: Parser[Expression]  =  funCall | literal | "("~>expression<~")"

  def literal:Parser[Expression] = boole |  inexact | exact | chars | identifier

  def boole:Parser[Boole] = "true|false".r ^^ {
    case "true" => Boole(true)
    case "false" => Boole(false)
  }
  def exact:Parser[Exact] = "0|(\\+|-)?[1-9][0-9]*".r ^^ {
    case s => Exact(s.toInt)
  }
  def inexact:Parser[Inexact] = "(\\+|-)?[0-9]+\\.[0-9]+".r ^^ {
    case s => Inexact(s.toDouble)
  }
  def chars: Parser[Chars] = """\"[^"]+\"""".r ^^ {
    case characters => Chars(characters.substring(1, characters.length - 1))
  }

   def funCall: Parser[FunCall] = identifier ~ operands ^^ {
     case iden ~ op => FunCall(iden,op)
   }

  def operands: Parser[List[Expression]] = "(" ~ opt(expression ~ rep("," ~> expression)) ~ ")" ^^ {
    case "(" ~ None~ ")" => List()
    case "(" ~ Some(exp ~ Nil)~ ")" => List(exp)
    case "(" ~ Some(exp ~ more)~ ")" => exp::more
  }

}

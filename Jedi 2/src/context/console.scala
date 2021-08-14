package context

import value._
import scala.io._
/*
 * Notes:
 * console is Jedi's user interface
 * parsers and global environment are created here
 * console.main launches repl or executes a Jedi file
 */
/**
 *
 *
 * def sum = add(mul(3, 4), sub(9, 1))
 * sum
 * 3 * 4 + 9
 * 3 * (4 + 9)
 * add(3, 4.0)
 * sub(3, 4)
 * sub(3, -4)
 * 3 - -4
 * def pi = 3.14
 * def e = 2.7
 * pi * e + sum
 * 3 < 4
 * 3 == 4
 * 3 != 4
 * 3 + (if(sum < 19) 8 else 3)
 * if (sum < 19) nada else 3
 * 3 < 4 && 1 + 1 == 2 && sum < 19 && nada
 * 4 < 3 || 1 + 1 != 2 || sum < 19 ||  true || nada
 * 3 +((1+2) + (1/2) * 5)
 * def string = "12345"
 * string + 567 + "wenbo"
 * less(3,2)
 * def x = more("123","456")
 * x
 * 3.5 + 1.2
 * (1+3) > (1+4)
 * x && true || false
 * add(4,"123")
 * g
 * The g can not be found
 * bye
 */
object console {
  val parsers = new Jedi1Parsers // for now
  val globalEnv = new Environment
  var verbose = false

  def execute(cmmd: String): String = {
    val tree = parsers.parseAll(parsers.expression, cmmd)
    tree match {
      case tree: parsers.Failure => throw new SyntaxException(tree)
      case _ => {
        val exp = tree.get // get the expression from the tree
        val result = exp.execute(globalEnv) // execute the expression
        result.toString // return string representation of result
      }
    }
  }

  private def executeFile(fileName: String) {

    var more = true

    for (line <- Source.fromFile(fileName).getLines if more) {
      try {
        if (!line.matches("""\s*""")) {
          println("-> " + line)
          println(execute(line))
        }
      } catch {

        case e: SyntaxException => {
          println(e)
          println(e.result.msg)
          println("line # = " + e.result.next.pos.line)
          println("column # = " + e.result.next.pos.column)
          println("token = " + e.result.next.first)
        }
        case e: UndefinedException => {
          println(e)
          if (verbose) e.printStackTrace()
        }
        case e: TypeException => {
          println(e)
          if (verbose) e.printStackTrace()
        }
        case e: JediException => {
          println(e)
          if (verbose) e.printStackTrace()
        }

        case e: Exception => {
          println(e)
          more = false
        }
      } // catch
    } // for
    println("bye")
  }


  // read-execute-print loop
  def repl {
    var more = true
    var cmmd = ""
    while (more) {
      try {
        print("-> ")
        cmmd = StdIn.readLine
        if (cmmd == "quit") more = false
        else println(execute(cmmd))
      }
      catch {
        case e: SyntaxException => {
          println(e)
          println(e.result.msg)
          println("line # = " + e.result.next.pos.line)
          println("column # = " + e.result.next.pos.column)
          println("token = " + e.result.next.first)
        }
        case e: UndefinedException => {
          println(e);
          if (verbose) e.printStackTrace()
        }
        case e: TypeException => {
          println(e)
          if (verbose) e.printStackTrace()
        }
        case e: JediException => {
          println(e);
          if (verbose) e.printStackTrace()
        }
        case e: Exception => {
          println(e.getMessage)
          more = false
        }
      } finally {
        Console.flush
      }
    }
    println("bye")
  }

  def main(args: Array[String]): Unit = {
    if (args.length == 0)
      repl
    else
      try {
        executeFile(args(0))
      } catch {
        case e: Exception => {
          println(e)
        }
      }
  }

}


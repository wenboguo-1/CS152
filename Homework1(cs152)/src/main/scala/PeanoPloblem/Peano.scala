package PeanoPloblem

import scala.util.control.Breaks.{break, breakable}
object Peano {
  var opr1 = 0.0
  var opr2 = 0.0
  def execute(cmmd: String): String = {
      val (operand1, operand2, sign) = getOutput(cmmd, "", "", "", 0)
      if (!isValidOperand(operand1, operand2))//Check if the two operands are valid
        throw new NumberFormat
      else if (sign != "*" && sign != "+")// check if the sign operator is valid
        throw new MissingOperator
      else if (sign == "+") // Else, return the calculation of two values in a string format
         String.format("%.2f",opr1 + opr2)
      else
        String.format("%.2f",opr1 * opr2)
  }
  //This function is to check if the two operands are valid
  def isValidOperand(operand1: String, operand2: String): Boolean = {
      var isValid = true
      try {
         opr1 = operand1.toDouble
         opr2 = operand2.toDouble
      }
       catch {
         case e: NumberFormatException => isValid = false
       }
      isValid
  }
  // This function is to get the output of the user by using recursion
  // it has three return values, one is the left operand and one is the right, and one is the sign operator
  def getOutput(input: String, operand1: String, operand2: String, sign: String, i: Int): (String, String, String) = {

    if (i == input.length)
       (operand1, operand2, sign)
    else if (input.charAt(i) == ' ') // ignore the space
       getOutput(input, operand1, operand2, sign, i + 1)
    else if (isSign(input.charAt(i))) { // check operators in the string
        if (operand1.isEmpty) //Avoid it skips the number like -876 or + 893
          getOutput(input, operand1 + input.charAt(i), operand2, sign, i + 1)
        else if (sign.isEmpty) //If operand1 is not empty and it encounters a operator,add the operator to sign
          getOutput(input, operand1, operand2, sign + input.charAt(i), i + 1)
        else // otherwise,assign it to second operand
          getOutput(input, operand1, operand2 + input.charAt(i), sign, i + 1)
    } else if (sign.isEmpty)
       getOutput(input, operand1 + input.charAt(i), operand2, sign, i + 1)
    else
       getOutput(input, operand1, operand2 + input.charAt(i), sign, i + 1)
  }

  //A boolean function that checks if the sign operator has been encountered
  def isSign(c: Char): Boolean = {
    if (c == '+' || c == '-' || c == '*' || c == '/' || c == '%' || c == '^')
      true
    else
      false
  }

  // read-execute-print loop
  def repl {
    println("Welcome to Peano 1.0")
    print("-> ")
    var input =  scala.io.StdIn.readLine()
    while (input != "quite") {
        try {
           println(execute(input))
         }catch {
          case e: NumberFormat => println(e)
          case e: MissingOperator => println(e)
        }
       print("-> ")
       input = scala.io.StdIn.readLine()
    }
    print("Bye")
  }


  def main(args: Array[String]): Unit = {
    repl
  }
}
//***************************************************//
     //OUTPUT
/*
    Welcome to Peano 1.0
-> -123+-123
-246.00
-> -123+123
0.00
-> 2.123++321.3
323.42
-> 2345-+342
PeanoPloblem.MissingOperator: Missing operator!
-> 374+sdad_
PeanoPloblem.NumberFormat: Illegal operand!
-> 12.34*-9823
-121215.82
-> 0-0
PeanoPloblem.MissingOperator: Missing operator!
-> 0+0
0.00
-> quite
Bye
 */
//*****************************************//



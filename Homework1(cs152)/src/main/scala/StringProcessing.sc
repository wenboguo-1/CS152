import scala.util.control.Breaks.breakable
import scala.util.control.Breaks.break
  //*****************************************//
  def isPal(str: String): Boolean = {
    var start = 0;
    var end = str.length - 1
    breakable {
      while (start < end) {
        breakable {
          while (start < end) { // ignore the space
            if (str.charAt(start) == ' ')
              start += 1
            else if (str.charAt(end) == ' ')
              end -= 1
            else
              break
          }
        }
        if (str.charAt(start) != str.charAt(end))
          break
        start += 1
        end -= 1
      }
    }
    if (start >= end)
      true
    else
      false
  }
//*****************************************************************//
println("Is \"$3.1441.3$\" a palindrome? ->" + isPal("$3.1441.3$") )
println("Is \" 121  \" a palindrome? ->" + isPal(" 121     ") )
println("Is \" GOOD BOY  \" a palindrome? ->" + isPal(" GOOD BOY ") )
//*********************************************************************//
            //OUTPUT
/*
         Is "$3.1441.3$" a palindrome? ->true
         Is " 121 " a palindrome? ->true
         Is " GOOD BOY  " a palindrome? ->false

 */

def isPal2(str: String): Boolean = {
    var start = 0
    var end = str.length - 1
    var isLetter1 = false
    var isLetter2 = false

   breakable {
     while (start < end) {
       while (!isLetter1 || !isLetter2) {
         isLetter1 = Character.isLetterOrDigit(str.charAt(start))
         isLetter2 = Character.isLetterOrDigit(str.charAt(end))
         if (!isLetter1)
           start += 1
         if (!isLetter2)
           end -= 1
       }
       if (Character.toLowerCase(str.charAt(start)) != Character.toLowerCase(str.charAt(end)))
          break
       start += 1
       end -= 1
       isLetter1 = false
       isLetter2 = false
     }
   }
   if(start >= end)
     true
   else
     false
  }
//*****************************************************************//
println("Is \"$3.1441.3$$$$\" a palindrome? ->" + isPal2("$3.1441.3$$$$") )
println("Is \" A man, a plan, a canal, Panama!    \" a palindrome? ->" + isPal2(" 121    ") )
println("Is \" GOOD BOY  \" a palindrome? ->" + isPal2(" GOOD BOY ") )
//*********************************************************************//
          //OOUTPUT
/*
         Is "$3.1441.3$$$$" a palindrome? ->true
         Is " A man, a plan, a canal, Panama!    " a palindrome? ->true
         Is " GOOD BOY  " a palindrome? ->false
 */

def mkWord(size: Int = 5): String = {
  val r = scala.util.Random
  var result = ""
  var walker = 0
  val letters = "abcdefghijklmnopqrstuvwxyz"

  while (walker < size) {
    result += letters.charAt(r.nextInt(26))
    walker += 1
  }

  result

}
//******************************************//
 println("mkWord of size 10 is  -> " + mkWord(10))
 println("mkWord of size 5  is -> " + mkWord())
 println("mkWord of size 21 size -> " + mkWord(21))
//******************************************//
     //OUTPUT
/*
   mkWord of size 10 is -> htegemzlhm
   mkWord of size 5 is  -> zbvfs
   mkWord of size 21 size -> hpdlfuggcvpxtcnoqmobq
 */

//******************************************//
def getStr(size: Int): String = {
  val r = scala.util.Random
  var result = ""
  var walker = 1
  val letters = "abcdefghijklmnopqrstuvwxyz"

  result += Character.toUpperCase(letters.charAt(r.nextInt(25)))
  while (walker < size) {
    result += letters.charAt(r.nextInt(26))
    walker += 1
  }

  result
}
def mySentence(size: Int = 10): String = {

  var res = ""
  var walker = 0
  val r = scala.util.Random
  while (walker < size) {
    val rSize = r.nextInt(15)
    res = res + getStr(rSize)
    res += " "
    walker += 1
  }
  res
}
//************************************//
println("My sentence is ->" + mySentence(15))
println("My sentence is ->" + mySentence())
println("My sentence is ->" + mySentence(3))
//*************************************//

      //OUTPUT
/*
     My sentence is ->Lvykexurdplul Swyybehflzhoo Yvsumvthqeji Jwai Kjkobp Hm Iv G Twov Kbndntxk Sowryxca Kgyindayergyp Kfsq Rnd Wuteko
     My sentence is ->Churbkzn V Ewpaznpfgiv Inxe P Nvghcl Moiww Ruhsyhxligcu Q Mqkb
     My sentence is ->Brunuxxkin Punrbisgauapiz Ckq
 */





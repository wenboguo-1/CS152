package Recognizers

trait Recognizers {
  var a = 0
  def matches(s: String): String => Boolean = {
    def r(s1: String) = s1.trim.equalsIgnoreCase(s)
    r _
  }

  // opt(r) = r?
  def opt(r: String => Boolean): String => Boolean = {
     def helper(s : String): Boolean ={
      if(s.isEmpty)
        true
      else
        r(s)
    }

    helper _
  }

  // pipe(r1, r2) = r1 | r2
  def pipe(r1: String => Boolean, r2: String => Boolean) : String => Boolean = {
    def helper(s:String) = r1(s) || r2(s)
    helper _
  }

  // follows(r1, r2) = r1 ~ r2
  def follows(r1: String => Boolean, r2: String => Boolean) : String => Boolean = {

    def r(s:String):Boolean = {
      var res  = false
      var index = 1
      while (!res && index <= s.length ) {
        while (index <= s.length && !res) { // check r1
          res = r1(s.take(index))
          index += 1
        }
        val str = s.drop(index - 1)
        if(index < s.length)
          res = false
        for (i <- index to s.length if !res) { // check r2
          res = r2(str.take(i))
        }
      }
       res
    }

    r _
  }

  // rep(r) = r*
  def rep(r: String => Boolean): String => Boolean = {

    //r1 uses recursion and iteration!
    def r1(s: String): Boolean = {
      var result = false
      if (s == "") result = true
      else {
        for(i <- 0 to s.length if !result)
          result = r(s.take(i)) && r1(s.drop(i))
      }
      result
    }
    r1 _
  }
}

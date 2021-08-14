def inc(n:Int): Int = {
  n+1
}
def dec(n:Int): Int ={
  n-1
}
def isZero(n:Int):Boolean={
  n==0
}

//*************************//
def add(n:Int, m:Int):Int = { //Normal recursion
  if( isZero(m))
    n
  else
    1 + add(n,dec(m))
}
def add2(n:Int,m:Int):Int={ // tail recursion
  def helper(num:Int,res:Int): Int={
    if(isZero(num)) {
      res + m
    }else
      helper(num-1,res+1)
  }
  helper(n,0)
}
println(add(1,13))
println(add2(1,13))
println(add(2,13))
println(add2(2,13))
/* Output
      14
      14
      15
      15
 */


//**************************//
def mul(n:Int,m:Int):Int = { // normal recursion
  if (isZero(dec(m)))
    n
  else
    n + mul(n, dec(m))
}
def mul2(n:Int,m:Int):Int = {

  def helper(num:Int,result:Int): Int ={ // tail recursion
    if(isZero(num))
      result
    else
      helper(dec(num),result+m)
  }
  helper(n,0)
}
println(mul(10,2))
println(mul2(10,2))
println(mul(10,6))
println(mul2(10,6))
/* Output

      20
      20
      60
      60
 */


//****************************//
def exp(m:Int): Int = { // normal recursion
  if(isZero(m))
    1
  else
    2 * exp(m-1)
}

def exp2(m:Int) = { // tail recursion

  def helper(num:Int,res:Int):Int = {
    if(num == m)
      res
    else
      helper(inc(num),2 * res)
  }
  helper(0,1)

}
print(exp(5))
print(exp2(5))
print(exp(10))
print(exp2(10))
/*
    Output
    32
    32
    1024
    1024

 */

//****************************//
def hyperExp(n: Int):Int = { //
  if( isZero(n))
    0
  else if (isZero(dec(n)))
    exp2(0)
  else
    exp2(hyperExp(n-1))
}
println(hyperExp(4))

def hyperExp2(n:Int):Int = {

  def helper(n1:Int,res:Int):Int = {
    if( n1 == n)
      res
    else
      helper(n1+1,exp2(res))
  }
  helper(0,0)
}
println(hyperExp(5))
println(hyperExp2(5))
println(hyperExp(4))
println(hyperExp2(4))
println(hyperExp(0))
println(hyperExp2(0))
/*
    Output
    65536
    65536
    16
    16
    0
    0
 */
//****************************************************************************
   /*
       First of all, the time complexity for the tail recursion is a bit better
       than that in normal recursion. For the normal recursion, when the program
       finishes all the stack function calls, it has to go back to calculate each
       value that each base case was returned. Take the add function for example,
       it takes about O(2n) time because after it takes about n time to  go through
       all the function calls, it also needs about extra n time to compute the result,
       so it takes about O(2n) time but still linear time for the upper bound. However,
       if we use tail recursion, we create an extra parameter to calculate the result while doing
       stack function call, so after we finish recursion, it doesn't have to computer it
       again, so it can avoid about n time compared to the normal recursion. For the
       Space complexity, I am not sure why tail recursion only takes constant space. I can
       not imagin because when we use recursion, we have to use internal stack right?
    */

//***********************************************************************************


//*************************//

def fib2(n:Int): Int = { // tail recursion
  if(n <= 1)
    n
  else {
    def helper(counter: Int, pre: Int, result: Int): Int = {
      if (counter >= n)
        result
      else {
        helper(inc(counter), result, result + pre)
      }
    }
    helper(2, 1, 1)
  }
}
def fib(n:Int): Int = { // normal recursion
   if(n <= 1)
     n
  else
    fib(n-2) + fib(n-1)
}
print(fib(6))
print(fib2(6))
print(fib(10))
print(fib2(10))
print(fib(7))
print(fib2(7))
/*
    Output
    8
    8
    55
    55
    13
    13
 */



//**********************************//
def choose(n: Int, m: Int): Int = { // normal recursion
  if( m > n )
    0
   else if (isZero(m) || m == n)
    1
  else
    choose(n - 1, m - 1) + choose(n - 1, m)
}
print(choose(5,3))
print(choose(5,1))
print(choose(5,5))
print(choose(10,3))
print(choose(10,5))
print(choose(10,8))
print(choose(10,11))
/*
    Output
    10
    5
    1
    120
    252
    45
    0
 */

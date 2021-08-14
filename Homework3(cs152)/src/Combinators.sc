def inc(x:Int):Option[Int] = Some(x + 1)
def double(x:Int):Option[Int] = Some(2 * x)
def id[T](x: T) = x
def log(x:Double):Option[Double] = if (x <= 0 ) None else Some(math.log(x))
def sqrt(x:Double):Option[Double] = if(x<0) None else Some(math.sqrt(x))

//****************************************************************//
def compose[T](f: T =>Option[T], g: T => Option[T]): T => Option[T] = {
  def h(x:T):Option[T] = {
    x match {
        case x if g(x) == None => None
        case x if f(g(x).get) == None => None
        case _ => f(g(x).get)
      }
  }
  h _
}

print(compose(sqrt _, log _)(0))
print(compose(sqrt _, log _)(5))
//****************OUTPUT*******************//
    /*

      None
      Some(1.2686362411795196)
     */

//*******************************************************************//
def selfIter[T](f:T=>Option[T],n:Int): T => Option[T] = {
  def h(x:T):Option[T] = {
    if (n == 0)
      Some(id(x))
    else {
      x match {
        case x if f(x) == None => None
        case _ => compose (f, selfIter(f, n - 1)) (x)
      }
    }
  }
  h _
}

println(selfIter(double _,3)(3))
println(selfIter(double _,3)(0))
println(selfIter(inc _,5)(7))
println(selfIter(log _,5 )(100000000))
println(selfIter(log _,5 )(0))
//****************OUTPUT*******************//
/*

    Some(24)
    Some(0)
    Some(12)
    Some(-2.7023552361749483)
    None

*/


//****************************************************//
def countPass[T](a:Array[T],test:T =>Boolean): Int={
  var result = 0
  for(ele <- a){
    if(test(ele))
      result+=1

  }
  result
}
def isBoolean[T](x:T) =  x.isInstanceOf[Boolean]
def isInteger[T](x:T) =  x.isInstanceOf[Integer]
def isDouble[T](x:T) = x.isInstanceOf[Double]
print(countPass(Array(1,2,"haha",true,false,3.4,20),isBoolean _))
print(countPass(Array(1,2,"haha",true,false,3.4,20),isInteger _))
print(countPass(Array(1,2,"haha",true,false,3.4,20),isDouble _))

//****************OUTPUT*******************//
/*

    2
    3
    1
*/

//****************************************************************//A
def recur(baseVal: Int, combiner: (Int, Int)=>Int): Int  => Int ={
       def f(n:Int):Int = {
            if( n == 0)
              baseVal
            else
              combiner(n,f(n-1))
       }
     f _
}
print(recur(1,(x,n:Int) => x * n)(6))
print(recur(1,(x,n:Int) => x * n)(10))
print(recur(1,(x,n:Int) => x * n)(3))
//****************OUTPUT*******************//
/*
      720
      3628800
      6
*/


//********************************************************//
def parseDigits(digits: String): Option[Int] =
  if (digits.matches("[0-9]*")) Some(digits.toInt) else None
def deOptionize(f:String => Option[Int]):String => Int ={
     def helper(str:String)= {
          if(f(str) == None)
            throw new Exception("Bad input");
          else
            str.toInt
     }
    helper _
}
print(deOptionize(parseDigits _)("12345"))
try{
    print(deOptionize(parseDigits _)("12x3444"))
}catch{
   case e:Exception => e.getMessage
}
//****************OUTPUT*******************//
/*
    12345
    val res14: Any = Bad input
*/

//*********************************************//

def combinator[T](init:T,f:T=>T):Int => T= {
       def h(n:Int): T = {
         var result = init
         for(i <- 0 to n) result = f(result)
         result
       }
   h _
}
print(combinator(2.0,(x:Double) => x * x)(3))
print(combinator(2.0,(x:Double) => x * x)(2))
print(combinator(2.0,(x:Double) => x * x)(4))
//****************OUTPUT*******************//
/*
    65536.0
    256.0
    4.294967296E9
*/


//*********************************************************//
def unitTest[T,S](f:T=>S,array:Array[Array[T]]): Int ={
   var res = 0
   for(i <- 0 to array.length - 1){
       if(f(array(i)(0)) == array(i)(1))
          res += 1
   }
  res
}

print(unitTest((x:Int)=> x*x*x,Array(Array(1,1),Array(2,8))))
print(unitTest((x:Int)=> x*x*x,Array(Array(1,1),Array(2,8),Array(2,12),Array(3,27))))

//****************OUTPUT*******************//
/*
    2
    3
*/

//*****************************************************************************//
def controlLoop[S](update:(S,Int) => S,state:S,cycle:Int,halt:(S,Int)=>Boolean):S = {
  if(halt(state,cycle))
    state
  else
    controlLoop(update ,update(state,cycle),cycle+1,halt)
}
//****************************************************************//
def sizeOfPopulation[S](n:Int): Int = {
  val limitPop = 100000
  def isExceeds(n:Int,cycle:Int) = n * 2 >= limitPop
  def doublePop (n:Int,cycle:Int) = n * 2
  controlLoop(doublePop _,n,0,isExceeds)
}

print(sizeOfPopulation(1))
print(sizeOfPopulation(23))
print(sizeOfPopulation(100))
//****************OUTPUT*******************//
/*
    65536
    94208
    51200
*/

//***********************************************************************//
def solve[S](f:Double=>Double):Double => Double = {
  val delta = 0.0000000001
  def r(n:Double):Double= {
    def isEnough (guss: Double, cycle: Int) =  math.abs(f(guss) - n) <= delta
    def improved (guss: Double, cycle: Int) = guss - (f(guss)-n) / prime (guss)
    def prime (x: Double) = ((f(x + delta) - n) - (f(x) - n))/delta
    controlLoop (improved _, 1.0, 0, isEnough)
  }
  r _
}

//*****************************************************************//
def squartRoot(x:Double):Double ={
  solve((x:Double) => x * x )(x)
}

print(squartRoot(6.0))
print(squartRoot(20.0))
print(squartRoot(13.0))
print(squartRoot(100.0))
//****************OUTPUT****************//
/*
    2.449489742786737
    4.47213595499971
    3.605551275463992
    10.0
 */

//******************************************//
def cubRoot(x:Double):Double = {
  solve((x:Double) => x*x*x)(x)
}


print(cubRoot(6.0))
print(cubRoot(20.0))
print(cubRoot(13.0))
print(cubRoot(100.0))
//****************OUTPUT****************//
/*
    1.8171205928321452
    2.7144176165949077
    2.351334687720727
    4.641588833612779
 */

//*******************************************//
def nthRoot(x: Double, n: Int):Double = {
  solve((x:Double)=> math.pow(x,n))(x)
}

print(nthRoot(6.0,2))
print(nthRoot(20.0,3))
print(nthRoot(13.0,2))
print(nthRoot(100.0,6))
//****************OUTPUT****************//
/*
    2.449489742786737
    2.7144176165949077
    3.605551275463992
    2.154434690031885
 */
//*********************************************//
def compoundInterest(time:Int):Double= {
  def isEnough(none:Double,cycle:Int) = cycle == time;
  def getRate():Double = 1.0/time;
  def getInterest(value:Double,cycle:Int) = value + getRate() * value
  controlLoop(getInterest,1.0,0,isEnough);
}
print(compoundInterest(12));
print(compoundInterest(365));
print(compoundInterest(355*24));
print(compoundInterest(355*24*60));
//****************OUTPUT****************//
/*
    2.613035290224677
    2.714567482021875
    2.7181223220387594
    2.71827916973745
 */

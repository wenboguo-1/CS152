package Recognizers
object RecognizerTests extends App with Recognizers {


  // ((00) ~ (111)* ~ (00)?) | (11111).
  var exp = pipe(follows(follows(matches("00"),rep(matches("111"))),opt(matches("00"))),matches("11111"))
  println(exp("00111111")) // = true
  println(exp("0011100"))
  println(exp("11111"))
  println(exp("110000011"))
}

/**
      OUTPUT
       true
      true
      true
      false
 */
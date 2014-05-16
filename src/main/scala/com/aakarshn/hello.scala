import scala.util.parsing.combinator._


package com.aakarshn {

  class Arith extends JavaTokenParsers {
    def expr :Parser[Any] = term~rep("+"~term|"-"~term)
    def term:Parser[Any] = factor~rep("*"~factor|"/"~factor)
    def factor:Parser[Any]= floatingPointNumber | "("~expr~")"
    def run(s:String) = parseAll(expr,s)
  }

  object RunArith extends Application {
    def parse(s:String) =  new Arith().run(s)
    println("Done")
  }
}

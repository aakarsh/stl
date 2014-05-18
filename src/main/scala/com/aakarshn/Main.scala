import scala.util.parsing.combinator._

package com.aakarshn {

  abstract class Term
  case class True extends Term
  case class False extends Term
  case class Zero extends Term
  case class Succ(t:Term) extends Term
  case class Pred(t:Term) extends Term
  case class If(t1:Term,t2:Term,t3:Term) extends Term
  case class IsZero(t:Term) extends Term

  class Arith extends RegexParsers{

    def value:Parser[Any] = (
      "0".r^^{_=> Zero } |
      "true".r^^{_=>True} |
      "false".r^^{_=>False}
    )

    def expr:Parser[Any] = term~rep(term|";"~term)
    def term:Parser[Any] = ( value  | 
      """iszero""".r~term 
      /**
      ^^{
        case ((iszero~Zero)~List()) => x
      } 
      */
      |
      "if".r~term~"then".r~"else".r~term ^^ {
        s => s
      } |
      "succ".r~term |
      "pred".r~term
    )
    def run(s:String) = parseAll(expr,s)

  }


  object RunArith extends Application {


    def eval1(t: Term):Term = {
      case  If(True(),t2,t3) => t2
      case  If(False(),t2,t3) => t3
      case  If(t1:Term,t2:Term,t3:Term) => If(eval1(t1),t2,t3)
    }

    def parse(s:String) =  new Arith().run(s)
    println("Done")
  }
}

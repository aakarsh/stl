package com.aakarshn

import scala.util.parsing.combinator._
import scala.io.Source
import java.io._

class LambdaParser extends RegexParsers {

  val SEMI =";"
  val NEWLINE ="\n"

  def SPACE:Parser[String] = " ".r
  def ZERO:Parser[String] = "0"
  def TRUE:Parser[String] = "true"
  def FALSE:Parser[String] = "false"
  def ID:Parser[String] =  "[a-z][A-Z0-9]*".r
  def LAMBDA:Parser[String] =  "lambda".r
  def DOT:Parser[String] ="."
  def IF:Parser[String] ="if".r
  def THEN:Parser[String] ="then".r
  def ELSE:Parser[String] ="else".r
  def ISZERO:Parser[String] ="iszero".r
  def SUCC:Parser[String] ="succ".r
  def PRED:Parser[String] ="pred".r
  def COMMA:Parser[String] =",".r
  def LPAREN:Parser[String] ="("
  def RPAREN:Parser[String] =")"


  def value:Parser[Term] = (
    ZERO^^{_=>  Zero() }      |
    TRUE^^{_=>  True()}   |
    FALSE^^{_=> False()}
  )

  def atomic:Parser[Term] = {
    ID ^^{
      s => s match{
        case _ =>{
          println("atom parser found :"+s)
          UnresolveVar(s)
        }
      }
    }
  }

  def lambda:Parser[Term] = {
    LAMBDA~>ID~DOT~term ^^ {s=>
      println("s:"+s)
      s match {
        case (v~d~body) => {
          println("Abstraction :"+v)
          Abs(v,body);
        }
      }
    }
  }

  def expr:Parser[List[Term]] = repsep(term_top,SEMI) | repsep(term_top,"\n")

  
  def term_app:Parser[Term] = (term ~SPACE~atomic ^^{s =>
    s match {
      case (t~s~a) => App(t,a)
    }}
    | atomic)


  def term_top:Parser[Term] = term^^{
    t:Term =>
    def walk(p:Term,ctx:List[String]):Term ={
      p match {
        case UnresolveVar(x) => {
          val index = ctx.indexOf(x)
          Var(index,ctx.length)
        }
        case App(t1:Term,t2:Term) => App(walk(t1,ctx),walk(t2,ctx))
        case Abs(name:String,body:Term) => Abs(name,walk(body,name::ctx))
        case t => t
      }
    }
    walk(t,List[String]())
  }

  def term:Parser[Term] = (
      atomic~COMMA~term^^ {
        case (a1~_~a2) => App(a1,a2)
      } 
      | LPAREN~> term <~RPAREN
      | lambda
      | value      
      | IF~term~THEN~term~ELSE~term ^^ {
          case(_~t1~_~t2~_~t3) => If(t1,t2,t3)
      }
      | ISZERO~term  ^^ {
        case(_~v) => IsZero(v)
      }
      | SUCC~term ^^ {
        case(_~v) => Succ(v)
      }
      | PRED~term ^^ {
        case(_~v) => Pred(v)
      }
      | atomic
  )

  def expression_parser = expr

  def fromReader(r:Reader):List[Term] = {
    parse(expr,r)  match  {
      case Success(result,_) => result
      case f: NoSuccess => scala.sys.error(f.msg)
    }
  }

  def fromString(s:String):List[Term] = {
    parseAll(expr,s) match  {
      case Success(result,_) => result
      case f: NoSuccess => scala.sys.error(f.msg)
    }
  }

  def fromString[T](p:Parser[T],s:String):T =
    parseAll(p,s) match  {
      case Success(result,_) => result
      case f: NoSuccess => scala.sys.error(f.msg)
   }

  def fromStringTerm(s:String):Term = fromString[Term](term,s)


}

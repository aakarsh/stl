package com.aakarshn

import scala.util.parsing.combinator._
import scala.io.Source
import java.io._

class LambdaParser extends RegexParsers {

  def value:Parser[Term] = (
    "0".r^^{_=>  Zero() }      |
      "true".r^^{_=>  True()}   |
      "false".r^^{_=> False()}
  )

  def atomic:Parser[Term] = {
    "[a-zA-Z0-9]+".r ^^{
      s => s match{
        case _ =>{
          println("atom parser found :"+s)
          UnresolveVar(s)
        }
      }
    }
  }

  def lambda:Parser[Term] = {
    "lambda ".r ~> "[a-zA-Z0-9]+".r~ ".".r~term ^^ {s=>
      println("s:"+s)
      s match {
        case (v~d~body) => {
          println("Abstraction :"+v)
          Abs(v,body);
        }
      }
    }
  }

  def expr:Parser[List[Term]] = repsep(term_top,";") | repsep(term_top,"\n")

  //      override def skipWhitespace = false
  def term_app:Parser[Term] = (term ~" ".r~atomic ^^{s =>
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
    atomic~" ".r~term^^ {s =>
      s match {
        case (a1~k~a2) => App(a1,a2)
      }} |
      "("~> term <~")"
      | lambda
      | value
      | atomic
      | "if".r~term~"then".r~term~"else".r~term ^^ {
        s => s match {
          case("if"~t1~"then"~t2~"else"~t3) => If(t1,t2,t3)
        }}
      | """iszero""".r~term  ^^ {
        case("iszero"~v) => IsZero(v)
      }
      |"succ".r~term ^^ {
        case("succ"~v) =>
          Succ(v)
      }
      |"pred".r~term ^^ {
        case("pred"~v) =>
          Pred(v)
      }
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
}

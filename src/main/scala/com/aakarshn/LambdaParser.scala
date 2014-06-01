package com.aakarshn

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.token._

import java.util.regex.Pattern
import scala.util.matching.Regex
import scala.util.parsing.input._

import scala.util.parsing.input.CharArrayReader.EofCh
import scala.collection.immutable.PagedSeq
import scala.util.parsing.input._

import scala.io.Source
import java.io._

class LambdaParser extends StdTokenParsers with ImplicitConversions  {

  type Tokens = LambdaLexer
  val lexical  = new Tokens

  import lexical.{Keyword,Scanner,Identifier,StringLit,NumericLit}

  def expr:Parser[List[Term]] = repsep(term_top,";") | repsep(term_top,"\n")
  
  def term_top:Parser[Term] = term^^{
    t:Term =>
    def walk(p:Term,ctx:List[String]):Term ={
      p match {
        case UnresolveVar(x) => {
          val index = ctx.indexOf(x)
          if(index >= 0)
            Var(index,ctx.length)
          else
            throw new RuntimeException("Unable to variable binder for :"+x+"\n In Contex "+ctx)
        }
        case App(t1:Term,t2:Term) => App(walk(t1,ctx),walk(t2,ctx))
        case Abs(name:String,body:Term) => Abs(name,walk(body,name::ctx))
        case t => t
      }
    }
    walk(t,List[String]())
  }

  def term:Parser[Term] = (
        app_term
      | number
      | var_term
      | string
      | true_term
      | false_term
      | if_term
      | succ
      | pred
      | lambda_term
      | "("~>term<~")" )

  def if_term = Keyword("if")~term~Keyword("then")~term~Keyword("else")~term ^^ {
      case (_~e1~_~e2~_~e3)  => If(e1,e2,e3)
  }

  def lambda_term = Keyword("lambda")~>ident~"."~term^^ {
    case (s~_~t) => Abs(s,t)
  }

  def true_term =   Keyword("true")^^^ True()
  def false_term = Keyword("false")^^^ False()
  
  def succ =  Keyword("succ")~term^^{ case (_~e) => Succ(e)  }
  def pred =  Keyword("pred")~term^^{ case (_~e) => Pred(e)  }

  def app_term = (
       ("("~>term<~")"
        | var_term
        | true_term
        | false_term) ~ term ^^ { case (v1 ~t ) => App(v1,t)})

  def var_term = accept("string",{
    case Identifier(s) => UnresolveVar(s)
  })

  def string = accept("string",{
    case StringLit(s) => StringTerm(s)
  })

  def number = accept("number",{
    case NumericLit(s) => 
      val n = s.toDouble
      if (n <= 0) Zero() 
      else NumberTerm(n.toDouble)
  })

  def parseRaw(input:String) = phrase(term)(new Scanner(input)) match {
    case Success(result,_) => Some(result)
    case f: NoSuccess => scala.sys.error(f.msg)    
  }

  def fromString(s:String):List[Term] = {
    phrase(expr)(new Scanner(s)) match  {
      case Success(result,_) => result
      case f: NoSuccess => scala.sys.error(f.msg)
    }
  }

  import scala.util.parsing.input.Reader;

  def fromReader (r: java.io.Reader) : List[Term] = {
    phrase(expr)(new Scanner(new PagedSeqReader(PagedSeq.fromReader(r)))) match  {
      case Success(result,_) => result
      case f: NoSuccess => scala.sys.error(f.msg)
    }
  }

  def fromString[T](p:Parser[T],s:String):T =
    phrase (p)(new Scanner(s)) match  {
      case Success(result,_) => result
      case f: NoSuccess => scala.sys.error(f.msg)
   }

  def fromStringTerm(s:String):Term = fromString[Term](term,s)  
}

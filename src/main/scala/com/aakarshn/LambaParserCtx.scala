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
import Syntax._;

class LambdaParserCtx extends StdTokenParsers with ImplicitConversions  {

  type Tokens = LambdaLexer
  val lexical  = new Tokens

  import lexical.{Keyword,Scanner,Identifier,StringLit,NumericLit,SpecialChar}
  import Syntax._

  def expr:Parser[List[CtxTerm]] = rep(term<~semi.*) 
  /**
  def cmd:Parser[(CtxTerm,Context)] = term | ident~binder ^^ {
    {case (s~b) =>
        (ctx:Context) => { 
          (Bind(s,(ctx)), 
            addName(ctx,s))

    }
  }}
  */

/**
[error] /home/aakarsh/src/scala/stl/src/main/scala/com/aakarshn/LambaParserCtx.scala:54: type mismatch;
[error]  found   : (List[com.aakarshn.Syntax.Command], com.aakarshn.Syntax.Context)
[error]  required: com.aakarshn.Syntax.Context => (List[com.aakarshn.Syntax.Command], com.aakarshn.Syntax.Context)
[error]     (which expands to)  List[(String, com.aakarshn.Syntax.Binding)] => (List[com.aakarshn.Syntax.Command], List[(String, com.aakarshn.Syntax.Binding)])
[error]         (List[Command](),emptycontext)
[error]         

*/
  def cmds:Parser[CtxCmds] =   rep(cmd<~semi.*) ^^  {  
        lst =>  //Context=>(List[Command],Context)
        ctx:Context =>
       /*
        case (cmds:List[Command],ctx:Context)  =>
          x:Context => {

        cmds.foldLeft((List[Command](),ctx)){
          case ((cmd:Command,ctx:Context),(acc_cmds,acc_ctx)) =>
          val c,ct = cmd(ctx)
          (c::acc_cmds,ct)
        }

          }
        */
    (List[Command](),emptycontext)
  }




  def cmd:Parser[CtxCmd]=  (
        term^^{ case ctxTrm =>
                    ctx:Context =>
                        val t = ctxTrm(ctx)
                        (Eval(t),ctx)
        }
      | ident~binder^^{ case (s~ctxBind) =>
                             ctx:Context =>
                                  val b = ctxBind(ctx)
                                  (Bind(s,b), addName(ctx,s))
      })
  


  def semi = accept(SpecialChar(';'))


  def binder:Parser[CtxBind] = 
    (SpecialChar('\\')^^{
      case (_) => {(ctx:Context) => NameBinding() }}
    |SpecialChar('=')~term^^{
      case (_~t) =>{(ctx:Context) => TmAbbBind(t(ctx))}})

  def term_top:Parser[CtxTerm] = term /** ^^ {

    //ctx:Context=> 
    t:Term => {
    def walk(p:Term,ctx:List[String]): Term = {
      p match {
        case UnresolveVar(x) => {
          val index = ctx.indexOf(x)
          if(index >= 0)
            Var(index,ctx.length)
          else
            throw new RuntimeException("Unable to variable binder for :"+x+"\n In Context "+ ctx)
        }
        case App(t1:Term,t2:Term) => App(walk(t1,ctx),walk(t2,ctx))
        case Abs(name:String,body:Term) => Abs(name,walk(body,name::ctx))
        case Let(name:String,t2:Term,body:Term) => Let(name,walk(t2,name::ctx),walk(body,name::ctx)) 
        case t => t
      }
    }
    //hmmm

    {ctx:Context =>
      (walk(t,List[String]()))
    }}


  }      */


  def term:(Parser[CtxTerm]) = (
        app_term
      | number
      | var_term
      | string
      | true_term
      | false_term
      | if_term
      | succ
      | pred
      | iszero
      | lambda_term
      | let_term
      | "("~>term<~")" )

  def let_term = Keyword("let")~ident~elem(SpecialChar('='))~term~Keyword("in")~term ^^ {
    case(_~e1~_~e2~_~e3) =>
      {
        ctx:Context =>
        Let(e1,e2(ctx),e3(ctx))
      }
  }

  def if_term = Keyword("if")~term~Keyword("then")~term~Keyword("else")~term ^^ {
      case (_~e1~_~e2~_~e3)  => 
      {ctx:Context =>
         If(e1(ctx),e2(ctx),e3(ctx))}
  }

  def lambda_term = Keyword("lambda")~>ident~"."~term^^ {
    case (s~_~t) => 
      {ctx:Context =>
        val ctx1 = addName(ctx,s)
        Abs(s,t(ctx1))}
  }

  def true_term =   Keyword("true")^^^ ({ctx:Context => True()})
  def false_term = Keyword("false")^^^ ({ctx:Context => False()})
  
  def iszero =  Keyword("iszero")~term^^{ case (_~e) => {ctx:Context => IsZero(e(ctx))}  }
  def succ =  Keyword("succ")~term^^{ case (_~e) => {ctx:Context => Succ(e(ctx)) } }
  def pred =  Keyword("pred")~term^^{ case (_~e) => {ctx:Context => Pred(e(ctx)) } }

  //Need the folling associativiy f x y -> App(App(f,x),y)
  def app_term = ((
    "("~>term<~")" 
     | var_term 
     | true_term 
     | false_term)~ term ^^ { 
    case (v1~t) => 
      { ctx:Context => App(v1(ctx),t(ctx)) }
})

  def var_term = accept("string",{
    case Identifier(s) => 
      ctx:Context =>
         Var(name2Index(ctx,s),ctx.length)
      //UnresolveVar(s)
  })

  def string = accept("string",{
    case StringLit(s) => 
      ctx:Context => StringTerm(s)
  })

  def number = accept("number",{
    case NumericLit(s) => 
      ctx:Context =>
          val n = s.toDouble
          if (n <= 0) Zero()
          else NumberTerm(n.toDouble)
  })

  def parseRaw(input:String, ctx:Context): Option[CtxTerm] =  phrase(term)(new Scanner(input)) match {
    case Success(result,_) => Some(result)
    case f: NoSuccess => scala.sys.error(f.msg)
  }

  def fromString(s:String,ctx:Context) : List[CtxTerm] = {
    phrase(expr)(new Scanner(s)) match  {
      case Success(result,_) => result
      case f: NoSuccess => scala.sys.error(f.msg)
    }
  }

  def fromReader (r: java.io.Reader,ctx:Context) : (List[CtxTerm]) = {
    phrase(expr)(new Scanner(new PagedSeqReader(PagedSeq.fromReader(r)))) match  {
      case Success(result,_) => result
      case f: NoSuccess => scala.sys.error(f.msg)
    }
  }

  def fromString[T](p:Parser[T],s:String,ctx:Context):T =
    phrase (p)(new Scanner(s)) match  {
      case Success(result,_) => result
      case f: NoSuccess => scala.sys.error(f.msg)
   }

  def fromStringTerm(s:String,ctx:Context):CtxTerm = fromString[CtxTerm](term,s,ctx)  

}

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

// Final Report : End  of next week

class LambdaParserCtx extends StdTokenParsers with ImplicitConversions  {

  type Tokens = LambdaLexer
  val lexical  = new Tokens

  import lexical.{Keyword,Scanner,Identifier,StringLit,NumericLit,SpecialChar}
  import Syntax._

  def expr:Parser[List[CtxTerm]] = rep(term<~semi.*) 

  def cmds:Parser[CtxCmds] =   rep(cmd<~semi.*) ^^  {  
        lst:List[CtxCmd] =>  
        ctx:Context =>
        def r(cmd:CtxCmd,ctx:Context,acc:List[Command]) ={
          val (rcmd,rctx)= cmd(ctx)
          (rcmd::acc,rctx)
        }
        var rctx = ctx;
        var rcmds = List[Command]();

        for(c <-lst) {
          val k = r(c,rctx,rcmds)
          rcmds = k._1
          rctx  = k._2
        }
       (rcmds,rctx)
  }


  def cmd:Parser[CtxCmd]=  (
        term^^{ case ctxTrm =>
                    ctx:Context =>
                        val (t,rctx) = ctxTrm(ctx)
                        (Eval(t),rctx)
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
      case (_~t) =>{(ctx:Context) => 
        val (rt,rctx) = t(ctx)
        (TmAbbBind(rt))
      }})

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
      | "("~>term<~")"
  )

  def let_term:Parser[CtxTerm] = Keyword("let")~ident~elem(SpecialChar('='))~term~Keyword("in")~term ^^ {
    case(_~e1~_~e2~_~e3) =>
      {
        ctx:Context =>
        var (r2,rctx) = e2(ctx)
        rctx = addName(ctx,e1)
        val (r3,_) = e3(rctx)
        (Let(e1,r2,r3),rctx)
      }
  }

  def if_term:Parser[CtxTerm] = Keyword("if")~term~Keyword("then")~term~Keyword("else")~term ^^ {
      case (_~e1~_~e2~_~e3)  => 
      {ctx:Context =>
         val (r1,_) = e1(ctx)
         val (r2,_) = e2(ctx)
         val (r3,_) = e3(ctx)
         (If(r1,r2,r3),ctx)
   }}

  def lambda_term:Parser[CtxTerm] = Keyword("lambda")~>ident~"."~term^^ {
    case (s~_~t) => 
      {ctx:Context =>
        val rctx = addName(ctx,s)
        val (rtm,rctx2) = t(rctx)
        println("adding name "+s);
        println("adding ctx "+ rctx2);
        (Abs(s,rtm),rctx2)
   }}

  def true_term:Parser[CtxTerm] =   Keyword("true")^^^ ({ctx:Context => (True(),ctx)})
  def false_term:Parser[CtxTerm] = Keyword("false")^^^ ({ctx:Context => (False(),ctx)})
  
  def iszero:Parser[CtxTerm] =  Keyword("iszero")~term^^{ case (_~e) => 
    { ctx:Context =>
       val (rterm,rctx) = e(ctx)
       (IsZero(rterm),rctx)
    }}

  def succ:Parser[CtxTerm] =  Keyword("succ")~term^^{ case (_~e) => {ctx:Context => 
    val (rterm,rctx) = e(ctx)
    (Succ(rterm),rctx) 
   }}

  def pred:Parser[CtxTerm] =  Keyword("pred")~term^^{ case (_~e) => {ctx:Context =>
    val (rterm,rctx) = e(ctx)
    (Pred(rterm),rctx)
  }}

  //Need the folling associativiy f x y -> App(App(f,x),y)
  def app_term:Parser[CtxTerm] = (
    ("("~>term<~")"| var_term | true_term | false_term )~term ^^ { 
    case (v1~t) => 
      { ctx:Context => 
             val (r1tm,_) = v1(ctx)
             val (r2tm,_) = t(ctx)
             (App(r1tm,r2tm),ctx)
      }
    })

  def var_term:Parser[CtxTerm] = accept("string",{
    case Identifier(s) => 
      ctx:Context =>
      val indx = name2Index(ctx,s)
      println("Saw Var :"+s+" Ctx"+ctx+"index "+indx)
        (Var(indx,ctx.length),ctx)
      //UnresolveVar(s)
  })

  def string:Parser[CtxTerm] = accept("string",{
    case StringLit(s) => 
      ctx:Context => (StringTerm(s),ctx)
  })

  def number:Parser[CtxTerm] = accept("number",{
    case NumericLit(s) => 
      ctx:Context =>
          val n = s.toDouble
          if (n <= 0) (Zero(),ctx)
          else (NumberTerm(n.toDouble),ctx)
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

  def parseCommands(s:String,ctx:Context) : CtxCmds = {
    phrase(cmds)(new Scanner(s)) match  {
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

  def fromString[T](p:Parser[T],s:String):T =
    phrase (p)(new Scanner(s)) match  {
      case Success(result,_) => result
      case f: NoSuccess => scala.sys.error(f.msg)
   }

  def fromStringTerm(s:String):CtxTerm = fromString[CtxTerm](term,s)  

}
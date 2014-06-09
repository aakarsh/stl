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

class LambdaParser extends StdTokenParsers with ImplicitConversions  {

  type Tokens = LambdaLexer
  val lexical  = new Tokens

  val debug = true

  import lexical.{Keyword,Scanner,Identifier,StringLit,NumericLit,SpecialChar}
  import Syntax._

  def cmds:Parser[List[CtxCmd]] = rep(cmd<~SEMICOLON.*)

  def cmd:Parser[CtxCmd]=  
    ( (ident~binder)^^{
         case (s~ctxBind) => 
           ctx:Context =>
           val b = ctxBind(ctx)
           (Bind(s,b), addBinding(ctx,s,b))
//             addName(ctx,s))
    }
     | term^^{ 
         case subterm =>  toCtx(Eval,subterm)
     })

  /// What is TmAbbABind supposed to do
  def binder:Parser[CtxBind] = 
     (SLASH ^^ {
       case (_) =>{(ctx:Context) =>
        if (debug) println("[debug]matched slash for name binding ")
        NameBinding()
       }}
    | EQ~term^^{
      case (_~t) =>{
        (ctx:Context) =>
        val (rt,rctx) = t(ctx)
        (TmAbbBind(rt,TyAny()))
      }})

  def expr:Parser[List[CtxTerm]] = rep(term<~SEMICOLON.*) 

  def term:(Parser[CtxTerm]) = (
        app_term
      | number
      | var_term
      | string
      | base_value
      | if_term
      | succ
      | pred
      | fix
      | iszero
      | lambda_term
      | let_term
      | "("~>term<~")"
  )

  def let_term:Parser[CtxTerm] = 
    Keyword("let")~ident~elem(SpecialChar("="))~term~Keyword("in")~term ^^ {
    case(_~idName~_~e2~_~e3) =>
      {
        ctx:Context =>
        var (r2,rctx) = e2(ctx)
        rctx = addName(ctx,idName)
        val (r3,_) = e3(rctx)
        (Let(idName,r2,r3),rctx)
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

  def atomic_type_parser:Parser[Type] = (
     "("~> arrow_type_parser <~")"
    |  accept("Type",{
              case Keyword("Bool") => TyBool()
              case Keyword("Nat") => TyNat()
              case Keyword("Unit") => TyUnit()
              case Keyword("Float") => TyFloat()
              case Keyword("String") => TyString()
  }))


  def arrow_type_parser:Parser[Type] = (      
    atomic_type_parser~SpecialChar("->")~arrow_type_parser ^^{
      case (ty1~_~ty2) => TyArrow(ty1,ty2)}
    |  atomic_type_parser)

  def type_term:Parser[Type] = COLON~>arrow_type_parser 


  def lambda_term:Parser[CtxTerm] = Keyword("lambda")~>ident~(type_term.?)~"."~term^^ {
    case (s~ty~_~t) => 
      {ctx:Context =>
        val var_type = ty match {
          case (Some(t)) => t
          case (None) => TyAny()
        }

        val rctx = addNameWithType(ctx,s,var_type)
        val (rtm,rctx2) = t(rctx)

        if (debug) println("[debug] adding name "+s+"\n[debug] new context : "+ctx);
        if (debug) println("[debug] ctx "+ rctx2);

        (Abs(s,var_type,rtm),rctx2)
   }}

  def base_value = ( 
      parser_subterms_0("unit",Unit)
    | parser_subterms_0("true",True)
    | parser_subterms_0("false",False))


  def iszero:Parser[CtxTerm] =  parser_subterms_1("iszero",IsZero)
  def succ:Parser[CtxTerm] =   parser_subterms_1("succ",Succ)
  def pred:Parser[CtxTerm] =  parser_subterms_1("pred",Pred)
  def fix:Parser[CtxTerm] =  parser_subterms_1("fix",Fix)

  //Need the folling associativiy f x y -> App(App(f,x),y)
  //TODO make left associative
  def app_term:Parser[CtxTerm] = (
    ("("~>term<~")"| var_term | base_value )~term ^^ { 
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
        if(debug) println("[debug]Saw Var :"+s+" Ctx"+ctx+"index "+indx)
        (Var(indx,ctx.length),ctx)
  })

  def string:Parser[CtxTerm] = accept("string",{
    case StringLit(s) => toCtxTerm(StringTerm(s))
  })

  def number:Parser[CtxTerm] = accept("number",{
    case NumericLit(s) => 
          val n = s.toDouble
          if (n <= 0) toCtxTerm(Zero)
          else toCtxTerm(NumberTerm(n.toDouble))
  })

  /**
   *  Begin parsing interaface here.
   */ 
  def parseCommands(s:String) : List[CtxCmd] = withParser(cmds,s)
  def parseCommands(r: java.io.Reader) : List[CtxCmd] = withParser(cmds,r)
  def parseCommand(s:String) : CtxCmd = withParser(cmd,s) 
  def parseExpression(s:String,ctx:Context) : List[CtxTerm] =  withParser(expr,s)

  def fromStringTerm(s:String):CtxTerm = withParser(term,s)

  def withParser[T](p:Parser[T],s:String):T = withParser(p,new Scanner(s))

  def withParser[T] (p:Parser[T],r: java.io.Reader):T = 
    withParser(p,new Scanner(new PagedSeqReader(PagedSeq.fromReader(r))))


  def withParser[T] (p:Parser[T],s:Scanner) : T =  {
    phrase(p)(s) match  {
      case Success(result,_) => result
      case f: NoSuccess => scala.sys.error(f.msg)
    }
  }

  def parseFirstTerm(s:String) :Term = parseTermsN(s,0)

  def parseTermsN(s:String,n:Int) = parseTerms(s)(n)

  def parseTerms(s:String) :List[Term] = parse(s,emptycontext)

  def parse(s:String,ctx:Context):List[Term] =  {
    val lst:List[CtxTerm] = parseExpression(s,ctx)
    var rctx:Context = ctx;
    var rtms:List[Term] = List[Term]();
    for(c <- lst) {
      val k = c(rctx)
      rctx = k._2
      rtms = k._1::rtms
    }
    rtms.reverse
  }  

  /**
    For generates parser of which maps Keyword => Term
    eg. true,false
    */
  def parser_subterms_0(s:String,term_constructor:()=>Term) =
    Keyword(s)^^^{toCtxTerm(term_constructor)}


  /**
    Generates parser which maps Keyword subterm => Term 
    egs. iszero,pred,succ
    */
  def parser_subterms_1(s:String,term_constructor:Term=>Term) =
    Keyword(s)~term ^^ {
      case(_~subterm) => toCtxTerm(term_constructor,subterm)
    }

  def SEMICOLON = accept(SpecialChar(";"))
  def SLASH = accept(SpecialChar("/"))
  def EQ = accept(SpecialChar("="))
  def COLON = accept(SpecialChar(":"))
}

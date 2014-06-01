package com.aakarshn

import com.aakarshn._
import org.scalatest.FlatSpec
import org.scalatest.Assertions._

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.token._
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.syntactical._

import Evaluator._

class ParserSpec extends UnitSpec {

  val parser = new LambdaParser()

  "Term parser" should "parse lambda " in {
    val v = parser.fromStringTerm("lambda x. x")
    assert(Abs("x",UnresolveVar("x")) == v,"identity lambda parsed")
  }

  it should "parse simple nested lambda " in {
    val v = parser.fromStringTerm("lambda x. lambda z. x")
    assert(Abs("x",Abs("z",UnresolveVar("x"))) == v,"nested lambda 2")
  }

  it should "application parsing" in {
    val v =parser.fromStringTerm("x x x")
    println("Application parsing "+v)
  }

  it should "generate nameless representation " in {
    val v = parser.fromString("lambda x. lambda y. lambda z. x y z")
    assert(Abs("x",Abs("y",Abs("z",App(Var(2,3),App(Var(1,3),Var(0,3)))))) == v(0),"hmm")
    assert(Abs("x",App(Var(0,1),Var(0,1))) == parser.fromString("lambda x. x x")(0),"paring simple application")
    assert(App(Abs("x",Var(0,1)),True()) == parser.fromString("(lambda x. x) true")(0),"paring simple application")


  }

  it should "parse if" in {
    val v = parser.fromStringTerm("if true then 0 else 0")
    assert(v == If(True(),Zero(),Zero()),"failing in parsing got "+v)
  }

  it should "parse succ" in {
    val v = parser.fromStringTerm("succ succ 0")
    assert(v == Succ(Succ(Zero())),"failing in parsing got "+v)
    val k = parser.fromStringTerm("succ pred 0")
    assert(k == Succ(Pred(Zero())),"failing in parsing got "+k)
  }

  it should "parse simple" in {
    assert(Zero() == parser.fromStringTerm("0"),"Failed parsing value 0")
    assert(True() == parser.fromStringTerm("true"),"Failed parsing value true")
    assert(False() == parser.fromStringTerm("false"),"Failed parsing value false")
  }  

  /**
  it should "parse variable"  in {
    val p = parse1("x")
    println(p)
    assert(UnresolveVar("x") == p)
  }
   */


  "Parser" should "parse simple arithmetic expressions" in {
      assert(Succ(Pred(Zero())) == parse1("succ(pred(0))"),"()  evaluation not working")
      assert(Succ(Zero()) == parse1("succ 0"), "parsing atomic true not working")
  }

  it should "parse simple boolean" in {
    assert(True() == parse1("true"), "parsing atomic true not working")
  }

  "Runner" should "parse and run simple arihmetic" in{
      assert(Zero() == run1("succ pred 0"), "succ not working with pred")
      assert(Zero() == run1("pred succ 0"), "pred not working with succ")
  }

  it should "parse and run simple boolean" in{
      assert(List(True(),False()) == Evaluator.run("true;false\n"),"multi expression parsing not working")
      assert(Zero() == run1("if true then 0 else succ 0"), "if-true evaluation not working")
      assert(Succ(Zero()) == run1("if false then 0 else succ 0"),"if-false  evaluation not working")
  }

  "Parser" should "work" in {
    assert(List(Abs("x",Abs("y",Var(1,2)))) == new LambdaParser().fromString("(lambda x. lambda y. x)"),"parsing abstraction failing")
  }


  class LParser extends StdTokenParsers with ImplicitConversions{

    type Tokens = LambdaLexer
    val lexical  = new Tokens

    case class StringTerm(v:String) extends Term
    case class NumberTerm(v:Double) extends Term
    
    def parseRaw(input:String) = phrase(term)(new lexical.Scanner(input)) match {
      case Success(result,_) => Some(result)
      case _ => None
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
      | lambda_term 
      | "("~>term<~")" )


    def if_term = 
      lexical.Keyword("if")~term~lexical.Keyword("then")~term~lexical.Keyword("else")~term ^^{
        case (_~e1~_~e2~_~e3)  => If(e1,e2,e3)
     }

    def lambda_term = lexical.Keyword("lambda")~>ident~"."~term^^ {
      case (s~_~t) => Abs(s,t)
    }

    def true_term =   lexical.Keyword("true")^^^ True()
    def false_term = lexical.Keyword("false")^^^ False()
    
    def succ =  lexical.Keyword("succ")~term^^{ case (_~e) => Succ(e)  }
    def pred =  lexical.Keyword("pred")~term^^{ case (_~e) => Pred(e)  }

    def app_term = (
      (
        "("~>term<~")"
          | var_term
          | true_term
          | false_term)~term
        ^^ { case (v1 ~t ) => App(v1,t) }
    )
/*
    Some(App(Abs(x,Abs(y,Abs(f,App(UnresolveVar(f),App(UnresolveVar(x),UnresolveVar(y)))))),
         App(True(),True())))
 */    
    def var_term = accept("string",{
      case lexical.Identifier(s) => UnresolveVar(s)
    })

    def string = accept("string",{
      case lexical.StringLit(s) => StringTerm(s)
    })

    def number = accept("number",{
      case lexical.NumericLit(n) => NumberTerm(n.toDouble)
    })

  }

  "Parser" should "now trying lexer dependent parser" in {
    val parser = new LParser();
    assertResult(Some(parser.NumberTerm(10.0)),"got number parsed"){ parser.parseRaw("10") } 
    assertResult(Some(parser.StringTerm("hello")),"parsed string") {parser.parseRaw("\"hello\"")}
    assertResult(Some(If(parser.NumberTerm(0),parser.NumberTerm(0),parser.NumberTerm(0))),"Failed if parsing") {parser.parseRaw("if 0 then 0 else 0")}
    assertResult(Some(Abs("x",True())),"parsed string") {parser.parseRaw("lambda x. true")}
    assertResult(Some(Abs("x",UnresolveVar("x"))),"parsed string") {parser.parseRaw("lambda x. x")}
    assertResult(Some(App(UnresolveVar("x"),UnresolveVar("x"))),"[failing to successfully parse application x. x.]") 
        {parser.parseRaw("x x")}

    assertResult(Some(App(Abs("x",Abs("y",Abs("f",App(App(UnresolveVar("f"),UnresolveVar("x")),UnresolveVar("y"))))),App(True(),True()))),"[fixed old?]"){
      parser.parseRaw("((lambda x. lambda y. lambda f. (f x) y) true) true")
//      parser.parseRaw("((lambda x. lambda y. lambda f. f x y) true) true")
//      parser.parseRaw("f x y")
    }
  }

  "Lexer" should "tag string literals" in {
    val lexical = new LambdaLexer()
    val scanner  = new lexical.Scanner("true false if")
    println(scanner.first.getClass)
    assertResult(lexical.Keyword("true"),"scanner not getting first token"){scanner.first}
    assertResult(lexical.Keyword("false"),"scanner not getting second token"){scanner.rest.first}
    assertResult(lexical.Keyword("if"),"scanner not getting second token"){scanner.rest.rest.first}

//  TODO failing identifier
    assertResult(lexical.Keyword("lambda")){new lexical.Scanner("lambda x. true").first}
    assertResult(lexical.Identifier("xoxo")){new lexical.Scanner("lambda xoxo. true").rest.first}
    assertResult(lexical.Identifier("xoxo")){new lexical.Scanner("lambda xoxo. xoxo").rest.rest.rest.first}


//    assert(true ==new lexical.Scanner("true"))

  }

  /**
// BREAKING TEST
  it should "allow application of multiple value functions" in {
    parser.fromString("(lambda x. lambda y. lambda f. f x y) true true")
  }
  */
}



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
import Syntax._

class ParserSpec extends UnitSpec {

  val parser = new LambdaParser()

  "Parser" should "parse slash" in {
    val term = parser.parseCommands("x/;")
    println(term);
  }


  "Term parser" should "parse lambda " in {
    val term = parser.fromStringTerm("lambda x. x")(emptycontext)
    println(term);
  }


    
  it should "return eval" in {
    assertResult((Abs("x",TyAny(),Var(0,1)),List(("x",NameBinding()))),""){
      val term = parser.fromStringTerm("lambda x. x")(emptycontext)
      println(term)
      term
    }
  }


  it should "parse nested lambda" in { 

    val exp = Abs("y",TyAny(),Abs("x",TyAny(),App(Var(0,2),Var(1,2))))
    assertResult(exp,""){
      parser.parseFirstTerm("lambda y. (lambda x. x y)")
    }

  }


  it should "parse let " in {
    val exp = Let("x",NumberTerm(2.0),Var(0,1))
    assertResult(exp ,"hmm"){
      parser.parseFirstTerm("let x = 2 in x")
    }
  }


  it should "parse simple nested lambda " in {
    val t = parser.fromStringTerm("lambda x. lambda z. x")(emptycontext)
    assert((Abs("x",TyAny(),Abs("z",TyAny(),Var(1,2))),List(("z",NameBinding()), ("x",NameBinding())))  == t,"nested lambda 2")
   }



  it should "application parsing" in {
    val v =parser.fromStringTerm("x x x")
    println("Application parsing "+v)
  }


  it should "generate nameless representation " in {
    val v = parser.parseExpression("lambda x. lambda y. lambda z. x y z",emptycontext)
    assert(Abs("x",TyAny(),Abs("y",TyAny(),Abs("z",TyAny(),App(Var(2,3),App(Var(1,3),Var(0,3)))))) == v(0)(emptycontext)._1,"hmm")
    assert(Abs("x",TyAny(),App(Var(0,1),Var(0,1))) == parser.parseExpression("lambda x. x x",emptycontext)(0)(emptycontext)._1,"paring simple application")
    assert(App(Abs("x",TyAny(),Var(0,1)),True()) == parser.parseExpression("(lambda x. x) true",emptycontext)(0)(emptycontext)._1,"paring simple application")


  }

  it should "parse if" in {
    val v = parser.fromStringTerm("if true then 0 else 0")(emptycontext)._1
    assert(v == If(True(),Zero(),Zero()),"failing in parsing got "+v)
  }

  it should "parse succ" in {
    val v = parser.fromStringTerm("succ succ 0")(emptycontext)._1
    assert(v == Succ(Succ(Zero())),"failing in parsing got "+v)
    val k = parser.fromStringTerm("succ pred 0")(emptycontext)._1
    assert(k == Succ(Pred(Zero())),"failing in parsing got "+k)
  }


  it should "parse simple" in {
    assert(Zero() == parser.fromStringTerm("0")(emptycontext)._1,"Failed parsing value 0")
    assert(True() == parser.fromStringTerm("true")(emptycontext)._1,"Failed parsing value true")
    assert(False() == parser.fromStringTerm("false")(emptycontext)._1,"Failed parsing value false")
  }  


  it should "parse simple arithmetic expressions" in {
    assertResult(Succ(Pred(Zero())),"()  evaluation not working"){
      parser.parse("succ(pred(0))",emptycontext)(0)
    }

    assertResult(Succ(Zero()), "parsing atomic true not working"){
      parser.parse("succ 0",emptycontext)(0)

    }
  }

  it should "parse simple boolean" in {
    assertResult(True(), "parsing atomic true not working"){
      parser.parse("true",emptycontext)(0)
    }
  }

  it  should "work" in {
    assertResult(List(Abs("x",TyAny(),Abs("y",TyAny(),Var(1,2)))),"parsing abstraction failing"){
      parser.parse("(lambda x. lambda y. x)",emptycontext)
    }
  }

  it should "parse simple abstraction " in {
    assertResult(List(Abs("x",TyNat(),Var(0,1))),"parsing iimple abstraction fialing"){
      parser.parse("lambda x:Nat. x",emptycontext)
    }
  }

  it should "parse simple arrow type " in {
    val expected =List(Abs("x",TyArrow(TyNat(),TyNat()),Var(0,1)))
    assertResult(expected,"arrow type"){
      parser.parse("lambda x:Nat->Nat . x",emptycontext)
    }
  }

  it should "type parens " in {
    val expected =List(Abs("x",TyArrow(TyNat(),TyNat()),Var(0,1)))
    assertResult(expected,"arrow type"){
     parser.parse("lambda x:(Nat->Nat) . x",emptycontext)
    }
  }

  it should "nested arrows " in {
    val expected = List(Abs("x",TyArrow(TyArrow(TyNat(),TyNat()),TyNat()),Var(0,1)))
    assertResult(expected,"arrow type"){
     parser.parse("lambda x:(Nat->Nat)->Nat . x",emptycontext)
    }
  }


  it should "now trying lexer dependent parser" in {
    val parser = new LambdaParser();
    assertResult(NumberTerm(10.0),"got number parsed"){ parser.parseFirstTerm("10") } 
    assertResult(StringTerm("hello"),"parsed string") {parser.parseFirstTerm("\"hello\"")}
    assertResult(If(Zero(),Zero(),Zero()),"Failed if parsing") {parser.parseFirstTerm("if 0 then 0 else 0")}
    assertResult(Abs("x",TyAny(),True()),"parsed string") {parser.parseFirstTerm("lambda x. true")}
    assertResult(Abs("x",TyAny(),Var(0,1)),"parsed string") {parser.parseFirstTerm("lambda x. x")}
  }

  it should "undefined variables  " in {
    intercept[RuntimeException] {
      assertResult(App(UnresolveVar("x"),UnresolveVar("x")),"[failing to successfully parse application x. x.]")
      {parser.parseFirstTerm("x x")}
    }
  }


  it should "fail 3  " in   {
    val expected = App(App(Abs("x",TyAny(),Abs("y",TyAny(),Abs("f",TyAny(),App(Var(0,3),App(Var(2,3),Var(1,3)))))),True()),True())
    assertResult(expected,"[fixed old?]"){
      parser.parseFirstTerm("((lambda x. lambda y. lambda f. (f x) y) true) true")
      parser.parseFirstTerm("((lambda x. lambda y. lambda f. f x y) true) true")
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
    assertResult(lexical.Keyword("lambda")){ new lexical.Scanner("lambda x. true").first }
    assertResult(lexical.Identifier("xoxo")){new lexical.Scanner("lambda xoxo. true").rest.first}
    assertResult(lexical.Identifier("xoxo")){new lexical.Scanner("lambda xoxo. xoxo").rest.rest.rest.first}
  }


  it should "semicolon as seperator" in {
    assertResult(List(True(),True()),"") { 
      parser.parseTerms("true;true")
    }
  }

  it should "semicolon as end of input" in {
    assertResult(True(),"") { 
      parser.parseFirstTerm("true;")
    }
  }

  it should "parse let statement" in {
    assertResult(Let("x",True(),Var(0,1)),"") {
      parser.parseExpression("let x=true in x",emptycontext)(0)(emptycontext)._1
    }
  }

}



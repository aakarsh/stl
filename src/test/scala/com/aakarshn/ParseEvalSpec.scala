package com.aakarshn

import org.scalatest.FlatSpec
import org.scalatest.Assertions._

import Evaluator._
import Syntax._

/**
  Integration of parser and evaluator
*/
class ParseEvalSpec extends UnitSpec {

  val parser = new LambdaParserCtx()

  "Integration " should "parse identity lambda" in {
    assertResult(True(),"Running identity returns identity got"){
      runFirst("(lambda x. x) true")
    }

  }

  it should "parse true" in {
      assertResult(True(),"true is true"){
        runFirst("true;\n")
      }
  }

  it should "parse false" in {
      assertResult(False(),"false is false"){
        runFirst("false;\n")
      }
  }
  it should "parse true and false" in {
    val terms = parser.parse("true;false",emptycontext)
    assertResult(True(),"first parsed term is true "){terms(0)}
    assertResult(List(True(),False()),"multi expression parsing not working"){
      run("true;false\n")
    }

  }

  it should "parse and evaluate simple lambda calculus expressions" in {
    val v = run("(lambda x. x) true")
    assert(True() == v(0),"Running identity returns identity got:"+v )

    run("(lambda x. x x)")
    run("(lambda x. x x) (lambda x. x) (lambda x. x)")

    val pair = run("((lambda x. lambda y. lambda f. (f x) y ) false ) true")

//    val hd  = Evaluator.run("(lambda p. p (lambda  )")
//    println(pair)
  }


  it should "run simple simple-arithmetic.f file without any errores" in {
    //TODO use relative path
    val dir = "/home/aakarsh/src/scala/stl/src/main/resources/test-files/"
    val filename = dir+"simple-arithmetic.f"
    Evaluator.processFile(filename,emptycontext);
  }


  it should "parse and run simple arihmetic" in{
      assert(Zero() == runFirst("succ pred 0"), "succ not working with pred")
      assert(Zero() == runFirst("pred succ 0"), "pred not working with succ")
  }

  it should "parse and run simple boolean" in{
    //Values reversed????
    assert(Zero() == runFirst("if true then 0 else succ 0"), "if-true evaluation not working")
    assert(Succ(Zero()) == runFirst("if false then 0 else succ 0"),"if-false  evaluation not working")
  }


  def run(s:String):List[Term]  = {
    val terms = parser.parse(s,emptycontext)
    terms.map(Evaluator.evalTerm(_,emptycontext))
  }

  def runFirst(s:String):Term  = {
    run(s)(0);
  }
  
}



package com.aakarshn

import org.scalatest.FlatSpec
import org.scalatest.Assertions._

import Evaluator._

class ParserSpec extends UnitSpec {

  val parser = new LambdaParser()

  "Term parser" should "parse lambda " in {
    val v = parser.fromStringTerm("lambda x. x")
    require(Abs("x",UnresolveVar("x")) == v,"identity lambda parsed")
  }

  it should "parse simple nested lambda " in {
    val v = parser.fromStringTerm("lambda x. lambda z. x")
    require(Abs("x",Abs("z",UnresolveVar("x"))) == v,"nested lambda 2")
  }

  it should "application parsing" in {
    val v =parser.fromStringTerm("x,x,x")
    println("Application parsing "+v)
  }

  it should "parse if" in {
    val v = parser.fromStringTerm("if true then 0 else 0")
    require(v == If(True(),Zero(),Zero()),"failing in parsing got "+v)
  }

  it should "parse succ" in {
    val v = parser.fromStringTerm("succ succ 0")
    require(v == Succ(Succ(Zero())),"failing in parsing got "+v)
    val k = parser.fromStringTerm("succ pred 0")
    require(k == Succ(Pred(Zero())),"failing in parsing got "+k)
  }

  it should "parse simple" in {
    require(Zero() == parser.fromStringTerm("0"),"Failed parsing value 0")
    require(True() == parser.fromStringTerm("true"),"Failed parsing value true")
    require(False() == parser.fromStringTerm("false"),"Failed parsing value false")
  }  

  /**
  it should "parse variable"  in {
    val p = parse1("x")
    println(p)
    require(UnresolveVar("x") == p)
  }
   */

    /**
  "Parser" should "parse simple arithmetic expressions" in {
      require(Succ(Pred(Zero())) == parse1("succ(pred(0))"),"()  evaluation not working")
      require(Succ(Zero()) == parse1("succ 0"), "parsing atomic true not working")
  }
  it should "parse simple boolean" in {
    require(True() == parse1("true"), "parsing atomic true not working")
  }

  "Runner" should "parse and run simple arihmetic" in{
      require(Zero() == run1("succ pred 0"), "succ not working with pred")
      require(Zero() == run1("pred succ 0"), "pred not working with succ")
  }

  it should "parse and run simple boolean" in{
      require(List(True(),False()) == Evaluator.run("true;false\n"),"multi expression parsing not working")
      require(Zero() == run1("if true then 0 else succ 0"), "if-true evaluation not working")
      require(Succ(Zero()) == run1("if false then 0 else succ 0"),"if-false  evaluation not working")
  }

  "Parser" should "work" in {
    require(List(Abs("x",Abs("y",Var(1,2)))) == new LambdaParser().fromString("(lambda x. lambda y. x)"),"parsing abstraction failing")
  }

  */
  /**
  "An empty Set" should "have size 0" in {
    assert(Set.empty.size == 0)
  }

  it should "produce NoSuchElementException when head is invoked" in {
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }
  */
}



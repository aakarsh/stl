package com.aakarshn

import org.scalatest.FlatSpec
import org.scalatest.Assertions._

import Evaluator._

class ParserSpec extends UnitSpec {


  "Term Parser" should "parse values" in {

    val p = new LCParser()
    p.parseAll(term,"0")

    require(Zero() == parse1("0"),"Failed parsing value 0")
    require(True() == parse1("true"),"Failed parsing value true")
    require(False() == parse1("false"),"Failed parsing value false")
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
    require(List(Abs("x",Abs("y",Var(1,2)))) == new LCParser().fromString("(lambda x. lambda y. x)"),"parsing abstraction failing")
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



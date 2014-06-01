package com.aakarshn

import org.scalatest.FlatSpec
import org.scalatest.Assertions._


import Evaluator._


class EvaluatorSpec extends UnitSpec {

  "Numbers" should "be numerical"  in {
      assert(is_numerical(Zero()))
      assert(is_numerical(Succ(Zero())))
      assert(is_numerical(Pred(Zero())))
  }

  "Values" should "be values" in {
      assert(is_value(True()))
      assert(is_value(False()))
      assert(is_value(Succ(Zero())))
      assert(is_value(Pred(Zero())))
  }

  "Eval" should "work" in {
      // Evaluator tests
      assert(True() == eval(If(True(),True(),False())), "If did not evaluate true correctly")
      assert(False() == eval(If(False(),True(),False())), "If did not evaluate false correctly")
      assert(Succ(Zero()) == eval(Succ(Zero())), "succ evaluated incorrectly")
      assert(Succ(Zero()) == eval(Succ(If(False(),Succ(Zero()),Zero()))), "succ evaluated incorrectly")
      assert(True() == eval(IsZero(Zero())), "iszero is not evaluting correctly")
      assert(False() == eval(IsZero(Succ(Zero()))), "iszero is not evaluting correctly")
  }

  "Shifting" should "work" in {
      assert(Var(1,1) == Var(0,0).rshift(1),"term shift")
      assert(Abs("x",Var(0,1)) == Abs("x",Var(0,0)).rshift(1), "term shift abstraction")
  }

  "Identity Eval" should "work" in {
       val id_term = Abs("x",Var(0,0))
      assert(True() == eval(App(id_term,True())),"identiy eval is failing")
      assert(False() == eval(App(id_term,False())),"identiy eval is failing")
      assert(Succ(Zero()) == eval(App(id_term,Succ(Zero()))),"identity evaluation is failing")
  }

  "Runner" should "should parse and evaluate simple lambda calculus expressions" in {
    val v = Evaluator.run("(lambda x. x) true")
    assert(True() == v(0),"Running identity returns identity got:"+v )

    Evaluator.run("(lambda x. x x)")
    Evaluator.run("(lambda x. x x) (lambda x. x) (lambda x. x)")
//  Evaluator.run("(lambda x. x) ")
//  val pair = Evaluator.run("(lambda x. lambda y. lambda f. f x y ) true true")
//   println(pair)

  }





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



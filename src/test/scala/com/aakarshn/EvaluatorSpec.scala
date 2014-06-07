package com.aakarshn

import org.scalatest.FlatSpec
import org.scalatest.Assertions._
import Evaluator._
import Syntax._
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
      assert(True() == eval_empty(If(True(),True(),False())), "If did not bevaluate true correctly")
      assert(False() == eval_empty(If(False(),True(),False())), "If did not evaluate false correctly")
      assert(Succ(Zero()) == eval_empty(Succ(Zero())), "succ evaluated incorrectly")
      assert(Succ(Zero()) == eval_empty(Succ(If(False(),Succ(Zero()),Zero()))), "succ evaluated incorrectly")
      assert(True() == eval_empty(IsZero(Zero())), "iszero is not evaluting correctly")
      assert(False() == eval_empty(IsZero(Succ(Zero()))), "iszero is not evaluting correctly")
  }

  "Shifting" should "work" in {
      assert(Var(1,1) == Var(0,0).termShift(1),"term shift")
      assert(Abs("x",TyAny(),Var(0,1)) == Abs("x",TyAny(),Var(0,0)).termShift(1), "term shift abstraction")
  }

  "Identity Eval" should "work" in {
      val id_term = Abs("x",TyAny(),Var(0,0))
      assert(True() == eval_empty(App(id_term,True())),"identiy eval is failing")
      assert(False() == eval_empty(App(id_term,False())),"identiy eval is failing")
      assert(Succ(Zero()) == eval_empty(App(id_term,Succ(Zero()))),"identity evaluation is failing")
  }

  "Runner" should "parse and evaluate simple lambda calculus expressions" in {
    // basically we want to be able to look at the resultign term after evaluatign a term
    Evaluator.processString("(lambda x. x x)")
    Evaluator.processString("(lambda x. x x) (lambda x. x) (lambda x. x)")
    val pair = Evaluator.processString("((lambda x. lambda y. lambda f. (f x) y ) false ) true")

//    val hd  = Evaluator.run("(lambda p. p (lambda  )")
//    println(pair)

  }


  it should "run simple simple-arithmetic.f file without any errores" in {
    val dir = "/home/aakarsh/src/scala/stl/src/main/resources/test-files/"
    val filename = dir+"simple-arithmetic.f"
    Evaluator.processFile(filename,emptycontext);
  }

  
}



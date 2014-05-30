package com.aakarshn

import org.scalatest.FlatSpec
import org.scalatest.Assertions._

import Evaluator._

class EvaluatorSpec extends UnitSpec {

  "Numbers" should "be numerical"  in {
      require(is_numerical(Zero()))
      require(is_numerical(Succ(Zero())))
      require(is_numerical(Pred(Zero())))
  }

  "Values" should "be values" in {
      require(is_value(True()))
      require(is_value(False()))
      require(is_value(Succ(Zero())))
      require(is_value(Pred(Zero())))
  }

  "Eval" should "work" in {
      // Evaluator tests
      require(True() == eval(If(True(),True(),False())), "If did not evaluate true correctly")
      require(False() == eval(If(False(),True(),False())), "If did not evaluate false correctly")
      require(Succ(Zero()) == eval(Succ(Zero())), "succ evaluated incorrectly")
      require(Succ(Zero()) == eval(Succ(If(False(),Succ(Zero()),Zero()))), "succ evaluated incorrectly")
      require(True() == eval(IsZero(Zero())), "iszero is not evaluting correctly")
      require(False() == eval(IsZero(Succ(Zero()))), "iszero is not evaluting correctly")
  }

  "Shifting" should "work" in {
      require(Var(1,1) == Var(0,0).rshift(1),"term shift")
      require(Abs("x",Var(0,1)) == Abs("x",Var(0,0)).rshift(1), "term shift abstraction")
  }

  "Identity Eval" should "work" in {
          val id_term = Abs("x",Var(0,0))
      require(True() == eval(App(id_term,True())),"identiy eval is failing")
      require(False() == eval(App(id_term,False())),"identiy eval is failing")
      require(Succ(Zero()) == eval(App(id_term,Succ(Zero()))),"identity evaluation is failing")
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



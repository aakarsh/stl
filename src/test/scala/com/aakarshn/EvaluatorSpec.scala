package com.aakarshn

import java.io.File
import org.scalatest.FlatSpec
import org.scalatest.Assertions._
import Evaluator._
import Syntax._
class EvaluatorSpec extends UnitSpec {

  "Numbers" should "be numerical"  in {
      assert(Zero().isNat())
      assert(Succ(Zero()).isNat())
      assert(Pred(Zero()).isNat())
  }

  "Values" should "be values" in {
      assert(True().is_value())
      assert(False().is_value())
      assert(Succ(Zero()).is_value())
      assert(Pred(Zero()).is_value())
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




  it should "process test files without errors" in {
    val dir = "test/"
    val filename = dir+"test-1.f"
    val files = new File(dir).listFiles.filter(_.toString.endsWith(".f")).map(_.getPath).sortWith(_.compareTo(_) < 0)
    for(file<- files){

      try{
        println("Begun processing file "+file)
        Evaluator.processFile(file,emptycontext)
        println("Finished processing file "+file)
      }catch{
        case (ex:Exception) =>
          ex.printStackTrace()
          fail("Evaluator failed on file "+file);

      }
    }
//    Evaluator.processFile(filename,emptycontext);
  }

  
}



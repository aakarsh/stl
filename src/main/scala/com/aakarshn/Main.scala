import scala.util.parsing.combinator._

package com.aakarshn {

  object Evaluator extends Application {

    abstract class EvalException extends Throwable
    case class NoRulesApply(s:String) extends  EvalException

    abstract class Term
    case class Unit extends Term
    case class True extends Term
    case class False extends Term
    case class Zero extends Term
    case class Succ(t:Term) extends Term
    case class Pred(t:Term) extends Term
    case class If(t1:Term,t2:Term,t3:Term) extends Term
    case class IsZero(t:Term) extends Term

    def is_numerical(t:Term): Boolean = {
      t match {
        case Zero() => true
        case Succ(t) => is_numerical(t)
        case Pred(t) => is_numerical(t)
        case _ => false
      }
    }

    def is_value(t:Term): Boolean = {
      t match {
        case True() => true
        case False() => true
        case t if is_numerical(t) => true
        case _ => false
      }
    }

    class Arith extends RegexParsers {
      def value:Parser[Any] = (
        "0".r^^{_=> Zero }      |
          "true".r^^{_=>True}   |
          "false".r^^{_=>False}
      )

      def expr:Parser[Any] = term~rep(term|";"~term)

      def term:Parser[Any] = (
          value                                    |
          """iszero""".r~term                      |
          "if".r~term~"then".r~"else".r~term       |
          "succ".r~term                            |
          "pred".r~term )

      def run(s:String) = parseAll(expr,s)

    }

    def eval1(term: Term):Term = {

      def eval_numerical(t1:Term) = {
        val te = eval1(t1)
        require(is_numerical(te))
        te
      }

      term match {
        case  If(True(),t2,t3) => t2
        case  If(False(),t2,t3) => t3
        case  If(t1:Term,t2:Term,t3:Term)   => If(eval1(t1),t2,t3)
        case  Succ(t1)  =>  Succ(eval_numerical(t1))
        case  Pred(t1)  => Pred(eval_numerical(t1))
        case  IsZero(Zero()) => True()
        case  IsZero(Succ(t:Term)) => False()
        case  IsZero(Pred(t:Term)) => False()
        case  IsZero(t1) => IsZero(eval1(t1))
        case _ => throw NoRulesApply("Out of rules")
      }

    }

    def eval(term:Term):Term = {
      try {
        eval1(term)
      } catch{
        case ex:NoRulesApply => term
      }
    }

    def parse(s:String) =  new Arith().run(s)

    // Begin Assertions here

    //numerical tests
    require(is_numerical(Zero()))
    require(is_numerical(Succ(Zero())))
    require(is_numerical(Pred(Zero())))

    //value tests
    require(is_value(True()))
    require(is_value(False()))
    require(is_value(Succ(Zero())))
    require(is_value(Pred(Zero())))


    // Evaluator tests
    require(True() == eval(If(True(),True(),False())), "If did not evaluate true correctly")
    require(False() == eval(If(False(),True(),False())), "If did not evaluate false correctly")
    require(Succ(Zero()) == eval(Succ(Zero())), "succ evaluated incorrectly")
    require(Succ(Zero()) == eval(Succ(If(False(),Succ(Zero()),Zero()))), "succ evaluated incorrectly")
    require(True() == eval(IsZero(Zero())), "iszero is not evaluting correctly")
    require(False() == eval(IsZero(Succ(Zero()))), "iszero is not evaluting correctly")


    println("Done")

  }

}
import com.aakarshn._


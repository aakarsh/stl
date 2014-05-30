import scala.util.parsing.combinator._
import scala.io.Source
import java.io._

package com.aakarshn {


    abstract class EvalException extends Throwable

    case class NoRulesApply(s:String) extends  EvalException

    abstract class Term {
      /**
        Term substitution
        */
      def substitute(variable:Int,value:Term) : Term = term_substitute(variable,value,this)

      /**
        Perform top level that is value gets substituted for variable 0
        */
      def substitute(value:Term):Term  = {
        /**
          Shift vars in value make 0 free then substitute it into body
          */
        val substituted_body = this.substitute(0,value.rshift(1))
        // Now that 0 has been substituted
        // Shift back variables in the program body
        substituted_body.lshift(1)
      }

      /**
        Term shifting with cutoff
        */
      def rshift(d:Int,c:Int) = term_shift(d,c,this)
      /**
        Term shifting
        */
      def rshift(d:Int):Term = term_shift(d,0,this)

      def lshift(d:Int):Term = this.rshift(-1)


      def map_vars(onvar:(Int,Int,Int) => Term, c:Int, term:Term) = {
        /**
          Walk over AST.        
          */
        def walk(cutoff:Int, term:Term):Term  = term match {

          case Var(x:Int,n:Int) => {
            onvar(cutoff,x,n)
          }
          case Abs(name:String,body:Term) => {
            // Entering abstraction
            Abs(name,walk(cutoff+1, body))
          }
          case App(t1:Term,t2:Term) =>
            App(walk(cutoff,t1),
              walk(cutoff,t2))
          case t1:Term => t1
          case _ => throw NoRulesApply("map_vars :Failing in mapping")
        }
        walk(c, term)
      }


      /**
        Walk through the program AST.
        Increment variable indices by d 
        if they lie above the cutoff c

        d - variable index increment
        c - variable increment cutoff 
        t - program ast
        */
      def term_shift(d:Int,c:Int,t:Term) = {

        def on_vars(cutoff:Int,x:Int,n:Int):Term = {
          if(x >= cutoff){
            Var(x+d,n+d);
          } else {
            Var(x,n+d);
          }
        }
        map_vars(on_vars,c,t);
      }


      def term_substitute(j:Int,s:Term,t:Term) : Term = {
        def on_vars(cutoff:Int , x:Int,n:Int):Term = {
          if(x == j+cutoff){
            s.rshift(cutoff)
          } else {
            Var(x,n)
          }
        }
        map_vars(on_vars,0,t)
      }

      def term_substitute_top(s:Term,body:Term):Term = {
        /**
          Shift vars in s make 0 free then substitute it into body
          */
        val substituted_body = body.substitute(0,s.rshift(1))
        // Now that 0 has been substituted
        // Shift back variables in the program body
        substituted_body.lshift(1)
      }
    }

    case class Unit extends Term
    case class True extends Term
    case class False extends Term
    case class Zero extends Term
    case class Succ(t:Term) extends Term
    case class Pred(t:Term) extends Term
    case class If(t1:Term,t2:Term,t3:Term) extends Term
    case class IsZero(t:Term) extends Term
    // n - keep track of the context length
    // id - id of the variable used for substitution
    case class Var(id:Int,n:Int) extends Term
    case class UnresolveVar(x:String) extends Term
    case class Abs(name:String,body:Term) extends Term
    case class App(t1:Term, t2:Term) extends Term


}

package com.aakarshn 

import scala.util.parsing.combinator._
import scala.io.Source
import java.io._

object Syntax {

  abstract class EvalException extends Throwable

  case class NoRulesApply(s:String) extends  EvalException
  type CtxTerm = Context => (Term,Context)
  type CtxBind= Context => Binding

  type CtxCmd = Context=>(Command,Context)
  type CtxCmds = Context=>(List[Command],Context)

  abstract class Command;
  case class Eval(term:Term) extends Command
  case class Bind(variable:String,b:Binding) extends Command

  type TermHandler = Term => Any

  def decontext(ctxcmd:CtxCmd) = ctxcmd(emptycontext)
  def decontext(ctxcmd:CtxCmd,ctx:Context) = ctxcmd(ctx)

  abstract class Binding;
  case class NameBinding extends Binding;
  case class VarBinding extends Binding;
  case class TmAbbBind(t:Term) extends Binding;

  // String variable name
  // Binding - VarBinding or NameBinding

  // TODO think about makeing it a class??
  type Context = List[(String,Binding)];
  val emptycontext:Context = List[(String,Binding)]();

  def addBinding(ctx:Context,x:String,bnd:Binding):Context = (x,bnd)::ctx
  def addName(ctx:Context,x:String)= addBinding(ctx,x,NameBinding())
  def isNameBound(ctx:Context,x:String) = (ctx.filter({case (s,_) => x == s})).length > 0

  def pickFreshName(ctx :Context,x:String):(Context,String) =
    if (isNameBound(ctx,x)) {
      pickFreshName(ctx,x+"'")
    } else {
      (addName(ctx,x),x)
    }

  //TODO add failure message
  def index2Name(ctx:Context,n:Int):String =  ctx(n)._1

  def name2Index(ctx:Context,x:String):Int =
    ctx match {
      case Nil => throw new java.lang.RuntimeException("name " +x+" not found");
      case (s,_)::rest => if (x == s) 0 else 1+(name2Index(rest,x))
    }





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
  case class StringTerm(v:String) extends Term
  case class NumberTerm(v:Double) extends Term
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
  case class Let(x:String,t1:Term, t2:Term) extends Term



}


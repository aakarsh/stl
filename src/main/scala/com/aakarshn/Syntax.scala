package com.aakarshn 

import scala.util.parsing.combinator._
import scala.io.Source
import java.io._

object Syntax {

  abstract class EvalException extends Throwable

  case class NoRulesApply(s:String) extends  EvalException
  type CtxTerm = Context => (Term,Context)
  type CtxBind= Context => Binding

  implicit def fromCtxTermToCtxTerm2(ctxTerm:CtxTerm) :CtxTerm2 =  new CtxTerm2{
      def apply(ctx:Context):(Term,Context) = ctxTerm(ctx)
  } 

  implicit def fromCtxTerm2ToCtxTerm(ctxTerm:CtxTerm) :CtxTerm =  {
    ctx:Context =>{ ctxTerm(ctx)}
  }

  abstract class CtxTerm2  extends CtxTerm {
    def apply(in: Context): (Term,Context)

    /**
      Chains two contextual terms discarding the term produced by c1
      */
    def chainContexts(c2:CtxTerm):CtxTerm2 =new CtxTerm2 {
      def apply(ctx:Context) :(Term,Context) = {
        val (_,rctx) = this(ctx)
        val (rterm2,rctx2) = c2(rctx)
        (rterm2,rctx2)
      }
    }

    def >>(c2:CtxTerm2) = this.chainContexts(_)
  }



  /**
    Raises a term into a CtxTerm which leaves ctx unchanged
  */
  def toCtxTerm(term:Term):CtxTerm =  {ctx:Context => (term,ctx)}
  def toCtxTerm(term_constructor:()=>Term):CtxTerm = toCtxTerm(term_constructor())
  def toCtxTerm(term_constructor:Term=>Term,subterm:CtxTerm) = toCtx(term_constructor,subterm)

  def toCtx[R](term_constructor:Term=>R,subterm:CtxTerm) = {
    ctx:Context=> {
    val (term:Term,rctx:Context) = subterm(ctx)
    (term_constructor(term),rctx)
  }}

  /**
    Chains two contextual terms discarding the term produced by c1
   */
  def ctx_chain_(c1:CtxTerm,c2:CtxTerm):CtxTerm ={
    ctx:Context =>
      val (rterm,rctx) = c1(ctx)
      val (rterm2,rctx2) = c2(rctx)
      (rterm2,rctx2)
  }

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
  case class VarBinding(t:Type) extends Binding;
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

  abstract class Type {  

  def toPrettyPrint(): String ={
    this match {
      case  TyBool()=> "Bool"
      case  TyNat()=> "Nat"
      case  TyString()=> "String"
      case  TyFloat()=> "Float"
      case  TyUnit()=> "Unit"
      case  TyInert(ty)=> ty.toPrettyPrint
      case  TyAny()=> "Any"
      case  TyArrow(ty1:Type,ty2:Type)=>
        ty1.toPrettyPrint+"->"+ty2.toPrettyPrint
    }
  }

}
  case class TyBool extends Type;
  case class TyNat extends Type;
  case class TyFloat() extends Type;
  case class TyString extends Type;
  case class TyUnit extends Type;
  case class TyArrow(t1:Type,t2:Type) extends Type;
  case class TyInert(t1:Type) extends Type;
  case class TyAny() extends Type;

  abstract class Term {
    /**
      Term substitution
      */
    def substitute(varIndex:Int,valueTerm:Term):Term = {
      def on_vars(cutoff:Int , x:Int,n:Int):Term = {
        if(x == varIndex+cutoff){// hmm....
//        if(x == varIndex){
          valueTerm.termShift(cutoff)
        } else {
          Var(x,n)
        }
      }
      map_vars(on_vars,0)
    }

    /**
      Perform top level that is value gets substituted for variable 0
      */
    def substitute(value:Term):Term  = {
      /**
       Shift vars in value make 0 free then substitute it into body
      */
      val substituted_body = substitute(0,value.termShift(1))
      // Now that 0 has been substituted
      // Shift back variables in the program body
      substituted_body.termShift(-1)
    }

    /**
      Term shifting with cutoff
      Walk through the program AST.
      Increment variable indices by d 
      if they lie above the cutoff c

      d - variable index increment
      c - variable increment cutoff 
      */
    def termShift(d:Int,c:Int) = { 
      def on_vars(cutoff:Int,x:Int,n:Int):Term = {
        if(x >= cutoff){
          Var(x+d,n+d);
        } else {
          Var(x,n+d);
        }
      }
      map_vars(on_vars,c);
    }

    /**
      Term shifting
      */
    def termShift(d:Int):Term = termShift(d,0)

    def map_vars(onvar:(Int,Int,Int) => Term, c:Int):Term = map_vars(onvar,c,this);

    def map_vars(onvar:(Int,Int,Int) => Term, c:Int, term:Term):Term = {

      /**
        Walk over AST.        
        */
    def walk(cutoff:Int, term:Term):Term  = term match {

        case Var(x:Int,n:Int) => {
          onvar(cutoff,x,n)
        }

        case Abs(name:String,ty:Type,body:Term) => {
          // Entering abstraction
          Abs(name,ty,walk(cutoff+1, body))
        }
        case App(t1:Term,t2:Term) => App(walk(cutoff,t1), walk(cutoff,t2))
        case Fix(t1:Term) => Fix(walk(cutoff,t1))
        case t1:Term => t1
        case _ => throw NoRulesApply("map_vars :Failing in mapping")
      }

      walk(c, term)
    }

    def term_substitute_top(value:Term,body:Term):Term = {
      //Shift vars in s make 0 free then substitute it into body
      val substituted_body = body.substitute(0,value.termShift(1))
      // Now that 0 has been substituted
      // Shift back variables in the program body
      substituted_body.termShift(-1)
    }

    def is_value() : Boolean = {
      this  match {
        case Abs(_,_,_) => true
        case t if (is_numerical() || is_boolean()) => true
        case _ => false
      }
    }

    def is_boolean() : Boolean = {
      this match{
        case True()| False() => true
        case _ => false
      }
    }

    def is_numerical(): Boolean = {
      this match {
        case Zero() => true
        case NumberTerm(_) => true
        case Succ(t) => t.is_numerical()
        case Pred(t) => t.is_numerical()
        case _ => false
      }
    }

    def num_term():Int =  {
      def nt(acc:Int, n:Term):Int =
        n match {
          case Zero() => acc
          case NumberTerm(n:Double) => n.toInt
          case Succ(t1 :Term) => nt(acc+1,t1)
          case Pred(t1:Term)  => nt(acc-1,t1)
          case _ => throw NoRulesApply("num_term:Not a number")
        }
      nt(0,this)
    }

    def toPrettyPrint():String = {
      this match {
        case False() => "false"
        case True() => "true"
        case App(t1:Term,t2:Term) => t1.toPrettyPrint + " "+t2.toPrettyPrint
        case Abs(name:String,_,body:Term) => "lambda "+name+". "+body.toPrettyPrint
        case Var(x:Int,_) => x.toString
        case t:Term if is_numerical() => num_term().toString
        //for identifiers
        case Succ(t) => "succ ("+ t.toPrettyPrint+")"
        case Pred(t) => "pred ("+t.toPrettyPrint+")"
        case StringTerm(s) => "\""+s+"\""
        case x =>
          println("Unkown term "+x);
          throw NoRulesApply("print_result:Out of rules")
      }
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
  case class Abs(name:String,var_type:Type,body:Term) extends Term
  case class App(t1:Term, t2:Term) extends Term
  case class Let(x:String,t1:Term, t2:Term) extends Term
  case class Fix(t1:Term) extends Term


}


package com.aakarshn 

import scala.util.parsing.combinator._
import scala.io.Source
import java.io._
import Syntax._

/**
  A direct port of the evaluator for simply typed lambda calculus.    
  */
object Evaluator {

  val parser = new LambdaParser()
  val repl_promt = "[STL] $ "
  val debug = true;

  def evalTerm1(term:Term,ctx:Context): Term =  {

    def eval_numerical(t1:Term,ctx:Context) = {
      val result = evalTerm1(t1,ctx)
      require(result.is_numerical)
      result
    }

    term match {
      case  Var(i:Int,_) => 
        getBinding (ctx,i) match {
          case NameBinding() => {
            if(debug)
              println("[debug]Name binding found at index "+i+"possibly for "+index2Name(ctx,i)+"not sure what to do")
            Unit()
          }
          case VarBinding(tyT)  =>{
            if(debug)
              println("[debug]VarBinding found at index "+i+"possibly for "+index2Name(ctx,i)+"not sure what to do")
            Unit()
          }case TmAbbBind(v:Term,_) => {
            if (debug)
              println("[debug] TmAbbBind found at index "+i+" possibly for "+index2Name(ctx,i)+"returning "+v)
            v
          }
        }
      case  If(True(),t2,t3) => t2
      case  If(False(),t2,t3) => t3
      case  If(t1:Term,t2:Term,t3:Term)   => If(evalTerm1(t1,ctx),t2,t3)
      case  Succ(Pred(t1)) => t1
      case  Succ(t1)  =>  Succ(eval_numerical(t1,ctx))
      case  Pred(Succ(t1)) => t1
      case  Pred(t1)  => Pred(eval_numerical(t1,ctx))
      case  IsZero(Zero()) => True()
      case  IsZero(Succ(t:Term)) => False()
      case  IsZero(Pred(t:Term)) => False()
      case  IsZero(t1) => IsZero(evalTerm1(t1,ctx))
      case  Let(name:String,v2:Term,body:Term) if v2.is_value => {
        body.substitute(v2)
      }
      case  Let(name:String,t2:Term,body:Term) => {
        Let(name,evalTerm1(t2,ctx),body)
      }
      //Lambda Calculus
      case App(Abs(name:String,_,body:Term),v2) if v2.is_value => {
        if(debug)
          println("[debug] eval substituting for variable "+name)
        body.substitute(v2)
      }
      case App(v1:Term,t2:Term) if v1.is_value =>{
        App(v1,evalTerm1(t2,ctx))
      }
      case App(t1:Term,t2:Term) => {
        val r1 = evalTerm1(t1,ctx)
        App(r1,t2)
      }
      case fixed_term@Fix(fixed_body:Term) if fixed_body.is_value =>{
        fixed_body match {
          case Abs(_,_,abs_body) => {
            if(debug) 
              println("[debug]re-substituting fix body  "+fixed_body)

            var r = fixed_body.substitute(fixed_term)

            if(debug)
              println("[debug] new fix term "+fixed_term)
            r
          }
          case _ => 
            throw NoRulesApply("Fix passed in non abstraction term "+fixed_body)
        }
      }
      case Fix(t1:Term) => 
        Fix(evalTerm1(t1,ctx))
      case _ => throw NoRulesApply("Out of rules")
    }
  }

  def eval_empty(term:Term):Term = {
    evalTerm(term,emptycontext)
  }

  def evalTerm(term:Term,ctx:Context):Term = {
    try {
      val t = evalTerm1(term,ctx)
      evalTerm(t,ctx)
    } catch{
      case ex:NoRulesApply => term
    }
  }

  def typeof(term:Term,ctx:Context):Type = {
    term match {
      case Var(n:Int,_) => 
        getTypeFromContext(ctx,n)
      /*
         Base constants
       */
      case True()    => TyBool()
      case False()   => TyBool()
      case Unit()    => TyUnit()
      case Zero()    => TyNat()
      case StringTerm(t) => TyString()
      case NumberTerm(v:Double) => TyFloat()

      /*
         Built in language constructs
       */
      case If(t1:Term,t2:Term,t3:Term) =>
        val ty1 = typeof(t1,ctx)
        if(ty1 != TyBool()){
          if(debug)println("[debug] Warning: If conditional non boolean");
          TyAny()
        }
        val ty2 = typeof(t2,ctx)
        val ty3 = typeof(t3,ctx)
        if (ty2 != ty3){
          if(debug)println("[debug] Warning: Arms of conditional are not the same ty2:"+ty2+" ty3:"+ty3);
          TyAny()
        }else {
          ty2;
        }
      case Succ(t1)  => 
        val ty = typeof(t1,ctx)
        if(ty != TyNat()){
          if(debug) println("[debug]Warning :Type in Succ wrong "+ty)
          TyAny()
        }else{
          TyNat()
        }
      case Pred(t1)  => 
        val ty = typeof(t1,ctx)
        if(ty != TyNat()){
          if(debug)println("[debug] Warning: Type in Succ wrong "+ty)
          TyAny()
        }else{
          TyNat()
        }      
      case IsZero(t1)  => 
        val ty = typeof(t1,ctx)
        if(ty != TyNat()){
          if (debug)println("[debug] Warning:Type in Succ wrong "+ty)
          TyAny()
        }else {
          TyBool()
        }
      /*
        Lambda calculus constructs
       */
      case App(t1:Term,t2:Term) =>{
        val tyT1 = typeof(t1,ctx)
        val tyT2 = typeof(t2,ctx)
        tyT1 match {
          case TyArrow(ty11,ty12) =>
            // if argument type equal type of argument 
            // return result typen
            if (ty11 == tyT2) 
              ty12
            else {
              if(debug)println("[debug]Application : "+ty11+"argument type!= "+tyT2+"argumeent");
              return TyAny()
            }
          case _  => {
              if(debug) println("[debug]Applying non method "+tyT1);
              return TyAny()
          }
        }
      }
      /*
         Variables defined in abstractions get added to the context
      */
      case Abs(x,tyT1,body:Term) =>
        val rctx = addNameWithType(ctx,x,tyT1)
        if(debug) println("[debug] Adding to binding toContext \n"+rctx)
        val bodyType = typeof(body,rctx)
        TyArrow(tyT1,bodyType)
      /*
       All failures in finding out the type end up here.
      */
      case _ => 
        if(debug)println("[debug]Warning: Unknown type for " + term)
        TyAny()
    }
  }

  def processString(s:String):scala.Unit = 
    processString(s,emptycontext)

  def processString(s :String , ctx: Context) = {
    val cmds:List[CtxCmd] = parser.parseCommands(s);
    processCommandList(cmds,ctx)
  }

  def processFile(in_file:String):Context =   processFile(in_file,emptycontext)

  def processFile(in_file:String,ctx:Context):Context ={
    val reader  = new FileReader(in_file);
    val lst:List[CtxCmd] = parser.parseCommands(reader);
    processCommandList(lst,ctx)
  }


  def processCommandList(lst:List[CtxCmd],ctx:Context) :Context = { 

    var cmds = List[Command]()
    var rctx = ctx;

    for(ctxCmd <- lst){
      val (cmd,newctx)  = ctxCmd(rctx)
      cmds = cmd::cmds
      rctx = newctx
    }
    cmds.reverse

    //parsing context
    rctx =  ctx
    for(cmd <- cmds) {
      rctx = processCommand(cmd,rctx)
    }
    rctx
  }

  /**
    Returns context modified as a resutl of evaluating command
  */
  def processCommand(cmd:Command,ctx:Context): Context =  {
    if(debug){
      println("[debug] processCommand "+cmd)
      println("[debug] with Context  "+ctx)
    }
    cmd match {
      case Eval(t)  => {
        val ty = typeof(t,ctx)
        val t1 = evalTerm(t,ctx)
        print(t1.toPrettyPrint(ctx)+"  : "+ty.toPrettyPrint) 
        println()
        return ctx
      }
      case Bind(x,b) => {
        val binding1  = checkBinding(b,ctx)
        val binding = evalBinding(binding1,ctx)
        if (debug) println("[debug]Adding x:"+binding+"to ctx :[" +ctx+"]")
        return addBinding(ctx,x,binding)
      }
    }
  }

  def checkBinding(b:Binding,ctx:Context) ={
    b match{
      case TmAbbBind(t:Term,ty1:Type) => 
        val ty2 = typeof(t,ctx)
        if (ty1!= ty2){
          println("[debug] binding  went from type "+ty1+" to  "+ty2)
          TmAbbBind(t,ty2)
        } else{
          TmAbbBind(t,ty1)
        }
      case t => t
    }
  }


  def evalBinding(b:Binding,ctx:Context) ={
    b match {
      case TmAbbBind(t,ty) =>
        val t1 = evalTerm(t,ctx)
        TmAbbBind(t1,ty)
      case t => t
    }
  }

  def repl():scala.Unit = {
    var ok = true
    var ctx = emptycontext;
    while(ok) {
      print(repl_promt)
      val line = readLine();
      try{
        line match {
          case ":q" =>  ok = false;
          case ":l" =>  {
            val fname  = line.split(" ")(1)
            ctx = Evaluator.processFile(fname,ctx)
          }
          case _  => 
            ctx = processString(line,ctx)
        }
      }
      catch {
        case ex:Exception  => ex.printStackTrace()
        case ex:NoRulesApply  => ex.printStackTrace()
      }
    }
  }
}



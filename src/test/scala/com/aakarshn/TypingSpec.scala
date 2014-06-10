package com.aakarshn

import org.scalatest.FlatSpec
import org.scalatest.Assertions._
import Evaluator._
import Syntax._

class TypingSpec extends UnitSpec {

  "Typechecker" should "check basic values"  in {
    assertResult(TyBool(),"true"){  typeFirst("true;")    }
    assertResult(TyBool(),"false"){  typeFirst("false;")    }
    assertResult(TyNat(),"0"){  typeFirst("0;")    }
    //kind of messed up
    assertResult(TyNat(),"1"){  typeFirst("1;")    }
  }

  it should "check abstractions" in {
    assertResult(TyArrow(TyBool(),TyBool()),"boolean identity"){
      typeFirst("lambda x:Bool. x")
    }
  }

  def typecheck(s:String):List[Type]  = {
    val terms = parser.parse(s,emptycontext)
    terms.map(Evaluator.typeof(_,emptycontext))
  }

  def typeFirst(s:String) = typecheck(s)(0)


  
}



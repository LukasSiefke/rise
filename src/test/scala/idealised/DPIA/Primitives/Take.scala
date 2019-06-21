package idealised.DPIA.Primitives

import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import idealised.util.gen

class Take extends idealised.util.Tests {

  test ("Simple take example") {
    val e = fun(ArrayType(128, int))(a =>
      take(8)(a) |> mapSeq(fun(x => x)))

    gen.CProgram(e)
  }

  ignore ("Trigger TakeAcc acceptor translation, what should happen?") {
    val e = fun(ArrayType(128, int))(a => take(8)(a))

    gen.CProgram(e)
  }

}

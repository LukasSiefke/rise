package idealised.OpenCL.DSL.FunctionalPrimitives

import idealised.Core._
import idealised.DSL.untyped.{VisitAndRebuild, _}
import idealised.OpenCL.AddressSpace

abstract class To(f: Expr[ExpType -> ExpType],
                  input: DataExpr,
                  addressSpace: AddressSpace,
                  private val makeTo: (Expr[ExpType -> ExpType], DataExpr) => To,
                  private val makeToPhrase: (DataType, DataType,
                   Phrase[ExpType -> ExpType], Phrase[ExpType]) => idealised.OpenCL.FunctionalPrimitives.To)
  extends PrimitiveExpr {

  override def inferTypes(subs: ExpressionToPhrase.SubstitutionMap): Primitive[ExpType] = {
    import ExpressionToPhrase._
    val input_ = ExpressionToPhrase(input, subs)
    input_.t match {
      case ExpType(dt1_) =>
        val f_ = ExpressionToPhrase.setParamAndInferType(f, exp"[$dt1_]", subs)
        f_.t match {
          case FunctionType(ExpType(t1_), ExpType(dt2_)) =>
            if (dt1_ == t1_) {
              makeToPhrase(dt1_, dt2_, f_, input_)
            } else {
              error(dt1_.toString + " and " + t1_.toString, expected = "them to match")
            }
          case x => error(x.toString, "FunctionType")
        }
      case x => error(x.toString, "ExpType")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): DataExpr = {
    makeTo(VisitAndRebuild(f, fun), VisitAndRebuild(input, fun))
  }

}

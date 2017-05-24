package idealised.DSL.untyped.FunctionalPrimitives

import idealised.Core._
import idealised.DSL.untyped.{ExpressionToPhrase, VisitAndRebuild, _}
import lift.arithmetic._

final case class Iterate(k: Nat,
                         f: Expr[`(nat)->`[ExpType -> ExpType]],
                         array: DataExpr) extends PrimitiveExpr {

  override def inferTypes(subs: ExpressionToPhrase.SubstitutionMap): Primitive[ExpType] = {
    import ExpressionToPhrase._
    val array_ = ExpressionToPhrase(array, subs)
    array_.t match {
      case ExpType(ArrayType(m_, dt_)) =>
        f match {
          case NatDependentLambdaExpr(l, body: Expr[ExpType -> ExpType]) =>
            val b = ExpressionToPhrase.setParamAndInferType(body, exp"[$l.$dt_]", subs)
            val f_ = NatDependentLambda(l, b) //f.copy(body=b)
            f_.t match {
              case NatDependentFunctionType(_,
              FunctionType(ExpType(ArrayType(l_, dt1_)),
              ExpType(ArrayType(l_n, dt2_)))) =>
                if (l == l_ && dt1_ == dt_ && dt2_ == dt_) {

                  val n_ = l_n match {
                    case Prod(l__ :: Pow(n1_, Cst(-1)) :: Nil) if l__.equals(l) => n1_
                    case _ => error("???")//error(s"$l_n", s"${l_ / n}")
                  }

                  idealised.FunctionalPrimitives.Iterate(n_, m_, k, dt_, f_, array_)
                } else {
                  error(s"expected $l == $l_ && $dt1_ == $dt_ && $dt2_ == $dt_")
                }
              case ft => error(ft.toString, NatDependentFunctionType.toString)
            }
          case _ => error(f.toString, NatDependentLambda.toString)
        }
      case t_ => error(t_.toString, "ExpType(ArrayType)")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): DataExpr = {
    Iterate(fun(k), VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
  }

}

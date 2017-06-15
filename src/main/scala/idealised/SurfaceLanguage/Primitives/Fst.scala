package idealised.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.PrimitiveExpr
import idealised.{DPIA, OpenCL, SurfaceLanguage}
import idealised.SurfaceLanguage.Types._

final case class Fst(tuple: DataExpr, override val `type`: Option[DataType] = None)
  extends PrimitiveExpr
{


  override def convertToPhrase: DPIA.Phrases.Phrase[DPIA.Types.ExpType] = {
    tuple.`type` match {
      case Some(TupleType(dt1, dt2)) =>
        DPIA.FunctionalPrimitives.Fst(dt1, dt2, tuple.toPhrase[DPIA.Types.ExpType])
      case _ => throw new Exception("")
    }
  }

  override def inferType(subs: TypeInference.SubstitutionMap): Fst = {
    import TypeInference._
    val tuple_ = TypeInference(tuple, subs)
    tuple_.`type` match {
      case Some(TupleType(dt1_, dt2_)) =>
        Fst(tuple_, Some(dt1_))

      case x => error(expr = s"Fst($tuple_)",
        found = s"`${x.toString}'", expected = "(dt1, dt2)")
    }
  }

  override def visitAndRebuild(f: SurfaceLanguage.VisitAndRebuild.Visitor): DataExpr = {
    Fst(SurfaceLanguage.VisitAndRebuild(tuple, f), `type`.map(f(_)))
  }

  override def toString: String = s"$tuple._1"
}
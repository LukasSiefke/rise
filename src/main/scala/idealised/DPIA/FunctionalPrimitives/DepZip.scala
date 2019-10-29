package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA.{Phrases, _}

import scala.xml.Elem

final case class DepZip(n: Nat,
                        ft1: NatToData,
                        ft2: NatToData,
                        e1: Phrase[ExpType],
                        e2: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (n: Nat) ->: (ft1: NatToData) ->: (ft2: NatToData) ->:
      (e1 :: ExpType(DepArrayType(n, ft1), read)) ->:
        (e2 :: ExpType(DepArrayType(n, ft2), read)) ->:
          ExpType(DepArrayType(n, i => PairType(ft1(i), ft2(i))), read)

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    DepZip(f.nat(n), f.natToData(ft1), f.natToData(ft2), VisitAndRebuild(e1, f), VisitAndRebuild(e2, f))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String =
    s"(depZip ${PrettyPhrasePrinter(e1)} ${PrettyPhrasePrinter(e2)})"

  override def xmlPrinter: Elem =
    <depZip n={ToString(n)} dt1={ToString(ft1)} dt2={ToString(ft2)}>
      <lhs type={ToString(ExpType(DepArrayType(n, ft1), read))}>
        {Phrases.xmlPrinter(e1)}
      </lhs>
      <rhs type={ToString(ExpType(DepArrayType(n, ft2), read))}>
        {Phrases.xmlPrinter(e2)}
      </rhs>
    </depZip>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    ???
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(e1)(λ(ExpType(DepArrayType(n, ft1), read))(x =>
      con(e2)(λ(ExpType(DepArrayType(n, ft2), read))(y =>
        C(DepZip(n, ft1, ft2, x, y)) )) ))
  }
}
package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class AsVectorAcc(n: Nat,
                             m: Nat,
                             dt: ScalarType,
                             array: Phrase[AccType])
  extends AccPrimitive {

  override val t: AccType =
    (n: Nat) -> (m: Nat) -> (dt: ScalarType) ->
      (array :: acc"[$n.${VectorType(m, dt)}]") ->
        acc"[${n * m}.$dt]"

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    AsVectorAcc(fun.nat(n), fun.nat(m), fun.data(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def prettyPrint: String = s"(asVectorAcc ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <asVectorAcc n={ToString(n)}>
      {Phrases.xmlPrinter(array)}
    </asVectorAcc>
}

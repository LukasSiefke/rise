package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class Partition(n: Nat,
                           m: Nat,
                           lenF:NatToNatLambda,
                           dt: DataType,
                           array: Phrase[ExpType])
  extends ExpPrimitive {


  override val t: ExpType =
    (n: Nat) -> (m: Nat) -> (dt: DataType) ->
      (array :: exp"[$n.$dt]") -> exp"[$m.${NatToDataLambda(m, (i:NatIdentifier) => ArrayType(lenF(i), dt))}]"

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Partition(fun.nat(n), fun.nat(m), fun.natToNat(lenF), fun.data(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String = s"(partition $n $m $lenF ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <partition n={ToString(n)} m={ToString(m)} lenID={ToString(lenF)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </partition>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    ???
  }

  override def mapAcceptorTranslation(f: Phrase[ExpType -> ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] =
    ???

  override def continuationTranslation(C: Phrase[ExpType -> CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(array)(λ(exp"[$n.$dt]")(x => C(Partition(n, m, lenF, dt, x)) ))
  }
}
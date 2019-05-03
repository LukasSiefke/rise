package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class ForNat(n: Nat,
                        body: Phrase[`(nat)->`[CommandType]],
                        unroll:Boolean)
  extends CommandPrimitive {

  override val t: CommandType = {
    val k = body.t.n
    (n: Nat) -> (body :: t"($k:nat) -> comm") -> comm
  }
  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    ForNat(fun(n), VisitAndRebuild(body, fun), unroll)
  }

  override def prettyPrint: String = s"(forNat 0..$n ${PrettyPhrasePrinter(body)})"

  override def xmlPrinter: Elem =
    <forNat n={ToString(n)}>
      {Phrases.xmlPrinter(body)}
    </forNat>
}

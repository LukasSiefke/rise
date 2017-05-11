package idealised.LowLevelPrimitives

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._

import scala.xml.Elem

final case class Seq(c1: Phrase[CommandType],
                     c2: Phrase[CommandType])
  extends CommandPrimitive {

  override def typeCheck(): Unit = {
    import TypeChecker._
    (c1 :: comm) -> (c2 :: comm) -> comm
  }

  override def eval(s: Store): Store = {
    val s1 = OperationalSemantics.eval(s, c1)
    OperationalSemantics.eval(s1, c2)
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    Seq(VisitAndRebuild(c1, fun), VisitAndRebuild(c2, fun))
  }

  override def prettyPrint: String =
    s"(${PrettyPhrasePrinter(c1)}; ${PrettyPhrasePrinter(c2)})"

  override def xmlPrinter: Elem =
    <seq>
      <c1>
        {Core.xmlPrinter(c1)}
      </c1>
      <c2>
        {Core.xmlPrinter(c2)}
      </c2>
    </seq>
}
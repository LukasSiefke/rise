package CommandPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType._
import Rewriting.SubstituteImplementations

case class New(dt: DataType, f: Phrase[(ExpType x AccType) -> CommandType]) extends CommandPattern {

  override def typeCheck(): CommandType = {
    import TypeChecker._
    TypeChecker(f) match {
      case FunctionType(PairType(ExpType(d1), AccType(d2)), CommandType()) =>
        if (dt == d1 && d1 == d2) {
          CommandType()
        } else {
          error(dt.toString + ", " + d1.toString + ", and " + d2.toString, expected = "them to match")
        }
      case x => error(x.toString, FunctionType.toString + "(" + PairType.toString +
        "(" + ExpType.toString + "(A)," + AccType.toString + "(A))," + CommandType() + ")")
    }
  }

  override def eval(s: Store): Store = {
    val fE = OperationalSemantics.eval(s, f)
    val arg = IdentPhrase[ExpType x AccType](newName())
    val s1: Store = OperationalSemantics.eval(s + (arg.name -> 0), fE(arg))
    s1 - arg.name
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): CommandPattern = {
    New(dt, OperationalSemantics.substitute(phrase, `for`, f))
  }

  override def substituteImpl: Phrase[CommandType] = New(dt, SubstituteImplementations.applyFun(f))

  override def toC = {
    val fE = Lift.liftFunction(f)
    val v = IdentPhrase[ExpType x AccType](OperationalSemantics.newName())
    val dt = f.t.inT.t1.dataType
    v.t = PairType(ExpType(dt), AccType(dt))
    s"{\n${Printer.nameOf(dt)} ${v.name};\n${Printer.toC(fE(v))}; \n}"
  }

  override def prettyPrint: String = s"new $dt ${PrettyPrinter(f)}"

}
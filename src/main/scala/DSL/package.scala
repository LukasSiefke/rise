import Core.PhraseType._
import Core._
import ExpPatterns._
import AccPatterns._
import CommandPatterns._

import scala.language.implicitConversions

package object DSL {
  implicit class BinOps(lhs: Phrase[ExpType]) {
    def +(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.ADD, lhs, rhs)

    def -(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.SUB, lhs, rhs)

    def *(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.MUL, lhs, rhs)

    def /(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.DIV, lhs, rhs)

    def %(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.MOD, lhs, rhs)
  }

  implicit class BinOpsPatterns(lhs: ExpPattern) {
    def +(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.ADD, lhs, rhs)

    def -(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.SUB, lhs, rhs)

    def *(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.MUL, lhs, rhs)

    def /(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.DIV, lhs, rhs)

    def %(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.MOD, lhs, rhs)
  }

  implicit class CallLambda[T1 <: PhraseType, T2 <: PhraseType](fun: Phrase[T1 -> T2]) {
    // on-the-fly beta-reduction
    def apply(arg: Phrase[T1]) = Lift.liftFunction(fun)(arg)
    //def apply(arg: Phrase[T1]) = ApplyPhrase(fun, arg)
  }

  implicit class SequentialComposition(c1: Phrase[CommandType]) {
    def `;`(c2: Phrase[CommandType]): Phrase[CommandType] = Seq(c1, c2)
  }

  implicit class SequentialPatternComposition(c1: CommandPattern) {
    def `;`(c2: Phrase[CommandType]): Phrase[CommandType] = Seq(c1, c2)
  }

  implicit class Assignment(lhs: Phrase[AccType]) {
    def :=(rhs: Phrase[ExpType]) = Assign(lhs, rhs)
  }

  implicit class AssignmentPattern(lhs: AccPattern) {
    def :=(rhs: Phrase[ExpType]) = Assign(lhs, rhs)
  }

  implicit def toPair[T1 <: PhraseType, T2 <: PhraseType](pair: (Phrase[T1], Phrase[T2])): PairPhrase[T1, T2] = {
    PairPhrase(pair._1, pair._2)
  }

  implicit def toLiteral(i: Int): LiteralPhrase = LiteralPhrase(i)

  implicit class ExpPhraseExtensions(e: Phrase[ExpType]) {
    def _1() = Fst(e)

    def _2() = Snd(e)

    def `@`(index: Phrase[ExpType]) = Idx(e, index)
  }

  implicit class AccPhraseExtensions(a: Phrase[AccType]) {
    def `@`(index: Phrase[ExpType]) = IdxAcc(a, index)
  }

  implicit class VarExtensions(v: Phrase[VarType]) {
    def rd: Proj1Phrase[ExpType, AccType] = π1(v)
    def wr: Proj2Phrase[ExpType, AccType] = π2(v)
  }
}

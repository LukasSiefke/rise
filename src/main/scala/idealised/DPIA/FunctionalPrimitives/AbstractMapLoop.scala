package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationToImperative, TranslationContext}
import idealised.DPIA._
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._

abstract class AbstractMapLoop(n: Nat,
                               dt1: DataType,
                               dt2: DataType,
                               f: Phrase[ExpType ->: ExpType],
                               array: Phrase[ExpType])
  extends AbstractMap(n, dt1, dt2, f, array)
{
  def makeMapI(n: Nat, dt1: DataType, dt2: DataType,
               f: Phrase[ExpType ->: AccType ->: CommType],
               array: Phrase[ExpType],
               out: Phrase[AccType])
              (implicit context: TranslationContext): Phrase[CommType]

  def mapAcceptorTranslation(f: Phrase[ExpType ->: ExpType],
                             A: Phrase[AccType])
                            (implicit context: TranslationContext): Phrase[CommType] = {
    ???
  }
  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(array)(λ(exp"[$n.$dt1, $read]")(x =>
      makeMapI(n, dt1, dt2,
        λ(exp"[$dt1, $read]")(x => λ(acc"[$dt2]")(o => acc(f(x))(o))),
        x, A)))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    ???
    /* TODO should be removed
    `new`(dt"[$n.$dt2]", λ(exp"[$n.$dt2, $read]" x acc"[$n.$dt2]")(tmp =>
      acc(this)(tmp.wr) `;` C(tmp.rd) ))
    */
  }
}

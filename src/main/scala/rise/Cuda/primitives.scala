package rise.Cuda

import primitiveMacro.Primitive.primitive
import rise.core.Nat
import rise.core.TypeLevelDSL._
import rise.core.types._

object primitives {

  sealed trait Primitive extends rise.core.Primitive

  protected def mapTypeScheme: Type =
    implN(n =>
      implDT(s =>
        implDT(t => (s ->: t) ->: ArrayType(n, s) ->: ArrayType(n, t))
      )
    )

  @primitive case class MapBlock(dim: Char)(override val t: Type = TypePlaceholder)
    extends Primitive {
    override def typeScheme: Type = mapTypeScheme
  }

  @primitive case class MapGlobal(dim: Char)(override val t: Type = TypePlaceholder)
    extends Primitive {
    override def typeScheme: Type = mapTypeScheme
  }

  @primitive case class MapThreads(dim: Char)(override val t: Type = TypePlaceholder)
    extends Primitive {
    override def typeScheme: Type = mapTypeScheme
  }

  @primitive case class MapWarp(dim: Char)(override val t: Type = TypePlaceholder)
    extends Primitive {
    override def typeScheme: Type = mapTypeScheme
  }

  @primitive case class MapLane(dim: Char)(override val t: Type = TypePlaceholder)
    extends Primitive {
    override def typeScheme: Type = mapTypeScheme
  }

  @primitive case class ToFragmentA(layout: WmmaFragmentLayout.Value, n: Nat)(override val t: Type = TypePlaceholder)
    extends Primitive {
    override def typeScheme: Type =
      implN(m =>
        implN(k =>
          implST(dt =>
            nFunT(_ =>
              ArrayType(m, ArrayType(k, dt)) ->: WmmaAMatrix(m, n, k, dt, layout)
            )
          )
        )
      )
  }

  @primitive case class ToFragmentB(layout: WmmaFragmentLayout.Value, m: Nat)(override val t: Type = TypePlaceholder)
    extends Primitive {
    override def typeScheme: Type =
      implN(k =>
        implN(n =>
          implST(dt =>
            nFunT(_ =>
              ArrayType(k, ArrayType(n, dt)) ->: WmmaBMatrix(m, n, k, dt, layout)
            )
          )
        )
      )
  }

  @primitive case class ToFragmentAcc(layout: WmmaFragmentLayout.Value, k: Nat)(override val t: Type = TypePlaceholder)
    extends Primitive {
    override def typeScheme: Type =
      implN(m =>
        implN(n =>
          implST(dt =>
            nFunT(_ =>
              ArrayType(m, ArrayType(n, dt)) ->: WmmaAcc(m, n, k, dt)
            )
          )
        )
      )
  }

  @primitive case class FromFragment(layout: WmmaFragmentLayout.Value)(override val t: Type = TypePlaceholder)
    extends Primitive {
    override def typeScheme: Type =
      implN(m =>
        implN(n =>
          implN(k =>
            implST(dt =>
              nFunT(_ =>
                WmmaAcc(m, n, k, dt) ->: ArrayType(m, ArrayType(n, dt))
              )
            )
          )
        )
      )
  }

  @primitive case class GenerateFragment(m: Nat, n: Nat, k: Nat)(override val t: Type = TypePlaceholder)
    extends Primitive {
    override def typeScheme: Type =
      implST(dt =>
        dt ->: WmmaAcc(m, n, k, dt)
      )
  }

  @primitive case class TensorMMA(layoutA: WmmaFragmentLayout.Value, layoutB: WmmaFragmentLayout.Value)(override val t: Type = TypePlaceholder)
    extends Primitive {
    override def typeScheme: Type =
      implN(m =>
        implN(n =>
          implN(k =>
            implST(dt =>
              implST(dt2 =>
                WmmaAMatrix(m, n, k, dt, layoutA) ->:
                  WmmaBMatrix(m, n, k, dt, layoutB) ->:
                  WmmaAcc(m, n, k, dt2) ->: WmmaAcc(m, n, k, dt2)
              )
            )
          )
        )
      )
  }
}
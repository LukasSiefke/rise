package rise.Cuda

import primitiveMacro.Primitive.primitive
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

  @primitive case class ToFragmentA(layout: MatrixLayout)(override val t: Type = TypePlaceholder)
    extends Primitive {
    override def typeScheme: Type =
      implN(m =>
        implN(n =>
          implN(k =>
            implST(dt =>
              nFunT(_ =>
                ArrayType(m, ArrayType(k, dt)) ->: WmmaAMatrix(m, n, k, dt, layout)
              )
            )
          )
        )
      )
  }

  @primitive case class ToFragmentB(layout: MatrixLayout)(override val t: Type = TypePlaceholder)
    extends Primitive {
    override def typeScheme: Type =
      implN(m =>
        implN(n =>
          implN(k =>
            implST(dt =>
              nFunT(_ =>
                ArrayType(k, ArrayType(n, dt)) ->: WmmaBMatrix(m, n, k, dt, layout)
              )
            )
          )
        )
      )
  }

  @primitive case class ToFragmentAcc(layout: MatrixLayout)(override val t: Type = TypePlaceholder)
    extends Primitive {
    override def typeScheme: Type =
      implN(m =>
        implN(n =>
          implN(k =>
            implST(dt =>
              nFunT(_ =>
                ArrayType(m, ArrayType(n, dt)) ->: WmmaAcc(m, n, k, dt)
              )
            )
          )
        )
      )
  }

  @primitive case class FromFragment(layout: MatrixLayout)(override val t: Type = TypePlaceholder)
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

  @primitive case class GenerateFragment()(override val t: Type = TypePlaceholder)
    extends Primitive {
    override def typeScheme: Type =
      implN(m =>
        implN(n =>
          implN(k =>
            implST(dt =>
              dt ->: WmmaAcc(m, n, k, dt)
            )
          )
        )
      )
  }

  @primitive case class TensorMMA()(override val t: Type = TypePlaceholder)
    extends Primitive {
    override def typeScheme: Type =
      implML(layoutA =>
        implML(layoutB =>
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
        )
      )
  }

  @primitive case class ScaleFragment()(override val t: Type = TypePlaceholder)
    extends Primitive {
    override def typeScheme: Type =
      implFT(fragType =>
        implDT(dt =>
          (dt ->: dt) ->: fragType ->: fragType))
  }

  @primitive case class GlobalToShared()(override val t: Type = TypePlaceholder)
    extends Primitive {
    override def typeScheme: Type = implDT(t => t ->: t)
  }
}
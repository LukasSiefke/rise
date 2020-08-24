package rise.Cuda

import rise.Cuda.primitives._
import rise.core.DSL._
import rise.core._
import rise.core.semantics.HalfData
import rise.core.types.MatrixLayout
import rise.core.types.MatrixLayout._

object DSL {
  object mapBlock {
    def apply(): MapBlock = MapBlock('x')()
    def apply(e: Expr): Expr = MapBlock('x')()(e)
    def apply(dim: Char): Expr = MapBlock(dim)()
  }

  object mapGlobal {
    def apply(): MapGlobal = MapGlobal('x')()
    def apply(e: Expr): Expr = MapGlobal('x')()(e)
    def apply(dim: Char): Expr = MapGlobal(dim)()
  }

  object mapThreads {
    def apply(): MapThreads = MapThreads('x')()
    def apply(e: Expr): Expr = MapThreads('x')()(e)
    def apply(dim: Char): Expr = MapThreads(dim)()
  }

  object mapWarp {
    def apply(): MapWarp = MapWarp('x')()
    def apply(e: Expr): Expr = MapWarp('x')()(e)
    def apply(dim: Char): Expr = MapWarp(dim)()
  }

  object mapLane {
    def apply(): MapLane = MapLane('x')()
    def apply(e: Expr): Expr = MapLane('x')()(e)
    def apply(dim: Char): Expr = MapLane(dim)()
  }

  object toFragmentA {
    def apply(ldm: Nat): Expr = ToFragmentA(Row_Major)()(ldm)
    def apply(layout: MatrixLayout, ldm: Nat): Expr = ToFragmentA(layout)()(ldm)
  }

  object toFragmentB {
    def apply(ldm: Nat): Expr = ToFragmentB(Row_Major)()(ldm)
    def apply(layout: MatrixLayout, ldm: Nat): Expr = ToFragmentB(layout)()(ldm)
  }

  object toFragmentAcc {
    def apply(ldm: Nat): Expr = ToFragmentAcc(Row_Major)()(ldm)
    def apply(layout: MatrixLayout, ldm: Nat): Expr = ToFragmentAcc(layout)()(ldm)
  }

  object fromFragment {
    def apply(ldm: Nat): Expr = FromFragment(Row_Major)()(ldm)
    def apply(layout: MatrixLayout, ldm: Nat): Expr = FromFragment(layout)()(ldm)
  }

  def generateFragment: GenerateFragment = GenerateFragment()()
  def scaleFragment: ScaleFragment = ScaleFragment()()
  def globalToShared: GlobalToShared = GlobalToShared()()
  def tensorMMA: TensorMMA = TensorMMA()()

  def h(f: Float): Literal = literal(HalfData(f))
}
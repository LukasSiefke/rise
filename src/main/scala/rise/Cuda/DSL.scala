package rise.Cuda

import rise.Cuda.primitives._
import rise.core.DSL._
import rise.core._
import rise.core.types.WmmaFragmentLayout
import rise.core.types.WmmaFragmentLayout._

object DSL {
  object mapBlock {
    def apply(): MapBlock = MapBlock('x')()
    def apply(e: Expr): Expr = MapBlock('x')()(e)
    def apply(dim: Char): Expr = MapBlock(dim)()
  }

  object mapGrid {
    def apply(): MapGrid = MapGrid('x')()
    def apply(e: Expr): Expr = MapGrid('x')()(e)
    def apply(dim: Char): Expr = MapGrid(dim)()
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

  object fromFragment {
    def apply(n: Nat): Expr = FromFragment(Row_Major)()(n)
    def apply(layout: WmmaFragmentLayout.Value): FromFragment = FromFragment(layout)()
  }

  def toFragmentA(layout: WmmaFragmentLayout.Value, n: Nat): ToFragmentA = ToFragmentA(layout, n)()
  def toFragmentB(layout: WmmaFragmentLayout.Value, m: Nat): ToFragmentB = ToFragmentB(layout, m)()
  def toFragmentAcc(layout: WmmaFragmentLayout.Value, k: Nat): ToFragmentAcc = ToFragmentAcc(layout, k)()
  def generateFragment(m: Nat, n: Nat, k: Nat): GenerateFragment = GenerateFragment(m, n, k)()
  def tensorMMA: TensorMMA = TensorMMA()()
}
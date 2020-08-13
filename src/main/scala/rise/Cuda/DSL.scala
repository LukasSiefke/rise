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
    def apply(n: Nat): Expr = ToFragmentA(Row_Major, n)()
    def apply(layout: WmmaFragmentLayout.Value, n: Nat): ToFragmentA = ToFragmentA(layout, n)()
  }

  object toFragmentB {
    def apply(m: Nat): Expr = ToFragmentB(Row_Major, m)()
    def apply(layout: WmmaFragmentLayout.Value, m: Nat): ToFragmentB = ToFragmentB(layout, m)()
  }

  object toFragmentAcc {
    def apply(k: Nat): Expr = ToFragmentAcc(Row_Major, k)()
    def apply(layout: WmmaFragmentLayout.Value, k: Nat): ToFragmentAcc = ToFragmentAcc(layout, k)()
  }

  object fromFragment {
    def apply(n: Nat): Expr = FromFragment(Row_Major)()(n)
    def apply(layout: WmmaFragmentLayout.Value): FromFragment = FromFragment(layout)()
  }

  def generateFragment(m: Nat, n: Nat, k: Nat): GenerateFragment = GenerateFragment(m, n, k)()
  def tensorMMA: TensorMMA = TensorMMA(Row_Major, Row_Major)()
  def tensorMMA2: TensorMMA = TensorMMA(Row_Major, Col_Major)()
}
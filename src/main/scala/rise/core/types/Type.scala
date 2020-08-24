package rise.core.types

import rise.core._
import arithexpr.arithmetic.RangeAdd

sealed trait Type

case class TypeException(msg: String) extends Exception {
  override def toString = s"type exception: $msg"
}

object TypePlaceholder extends Type {
  override def toString = "?"
}

final case class TypeIdentifier(name: String)
    extends Type
    with Kind.Identifier {
  override def toString: String = "_" + name
}

final case class FunType[T1 <: Type, T2 <: Type](inT: T1, outT: T2)
    extends Type {
  override def toString: String = s"($inT -> $outT)"
}

final case class DepFunType[K <: Kind: KindName, T <: Type](
    x: K#I with Kind.Explicitness,
    t: T
) extends Type {
  override def toString: String =
    s"(${x.name}: ${implicitly[KindName[K]].get} -> $t)"

  override def equals(obj: Any): Boolean = obj match {
    case other: DepFunType[K, _] =>
      t == lifting.liftDependentFunctionType[K](other)(x)
    case _ => false
  }
}

sealed trait DataType extends Type

final case class DataTypeIdentifier(
    name: String,
    override val isExplicit: Boolean = false
) extends DataType
    with Kind.Identifier
    with Kind.Explicitness {
  override def toString: String = if (isExplicit) name else "_" + name
  override def asExplicit: DataTypeIdentifier = this.copy(isExplicit = true)
  override def asImplicit: DataTypeIdentifier = this.copy(isExplicit = false)
  override def equals(that: Any): Boolean = that match {
    case d: DataTypeIdentifier => this.name == d.name
    case _                     => false
  }
  override def hashCode(): Int = this.name.hashCode()
}

sealed trait ComposedType extends DataType

final case class ArrayType(size: Nat, elemType: DataType) extends ComposedType {
  override def toString: String = s"$size.$elemType"
}

final case class DepArrayType(size: Nat, fdt: NatToData) extends ComposedType {
  override def toString: String = s"$size.$fdt"
}

object DepArrayType {
  def apply(size: Nat, f: Nat => DataType): DepArrayType = {
    val n = NatIdentifier(freshName("n"), RangeAdd(0, size, 1))
    DepArrayType(size, NatToDataLambda(n, f(n)))
  }
}

final case class PairType(p1: DataType, p2: DataType) extends ComposedType {
  override def toString: String = s"($p1, $p2)"
}

sealed trait MatrixLayout

object MatrixLayout {
  object Row_Major extends MatrixLayout { override def toString = "Row_Major" }
  object Col_Major extends MatrixLayout { override def toString = "Col_Major" }
}

final case class MatrixLayoutIdentifier(
                                         name: String,
                                         override val isExplicit: Boolean = false
                                       ) extends MatrixLayout
  with Kind.Identifier
  with Kind.Explicitness {
  override def toString: String = if (isExplicit) name else "_" + name
  override def asExplicit: MatrixLayoutIdentifier = this.copy(isExplicit = true)
  override def asImplicit: MatrixLayoutIdentifier =
    this.copy(isExplicit = false)
  override def equals(that: Any): Boolean = that match {
    case a: MatrixLayoutIdentifier => this.name == a.name
    case _                         => false
  }
  override def hashCode(): Int = this.name.hashCode()
}

sealed trait WmmaFragment extends ComposedType {
  def m:Nat
  def n:Nat
  def k:Nat
  def dataType: DataType

  def arrayType: ArrayType
}

final case class WmmaAMatrix(m: Nat,
                             n: Nat,
                             k: Nat,
                             dataType: DataType,
                             layout: MatrixLayout
                            ) extends WmmaFragment {
  override def arrayType: ArrayType = ArrayType(m, ArrayType(k, dataType))

  override def toString: String = s"wmmaAMatrix[$m,$n,$k,$dataType $layout]"
}

final case class WmmaBMatrix(m: Nat,
                             n: Nat,
                             k: Nat,
                             dataType: DataType,
                             layout: MatrixLayout
                            ) extends WmmaFragment {
  override def arrayType: ArrayType = ArrayType(k, ArrayType(n, dataType))

  override def toString: String = s"wmmaBMatrix[$m,$n,$k,$dataType $layout]"
}

final case class WmmaAcc(m: Nat,
                         n: Nat,
                         k: Nat,
                         dataType: DataType
                        ) extends WmmaFragment {
  override def arrayType: ArrayType = ArrayType(m, ArrayType(n, dataType))

  override def toString: String = s"WmmaAccumulator[$m,$n,$k,$dataType]"
}

sealed trait BasicType extends DataType

sealed trait ScalarType extends BasicType

object bool extends ScalarType {
  override def toString: String = "bool"
}

object int extends ScalarType {
  override def toString: String = "int"
}

object i8 extends ScalarType { override def toString: String = "i8" }
object i16 extends ScalarType { override def toString: String = "i16" }
object i32 extends ScalarType { override def toString: String = "i32" }
object i64 extends ScalarType { override def toString: String = "i64" }

object u8 extends ScalarType { override def toString: String = "u8" }
object u16 extends ScalarType { override def toString: String = "u16" }
object u32 extends ScalarType { override def toString: String = "u32" }
object u64 extends ScalarType { override def toString: String = "u64" }

object f16 extends ScalarType { override def toString: String = "f16" }
object f32 extends ScalarType { override def toString: String = "f32" }
object f64 extends ScalarType { override def toString: String = "f64" }

object NatType extends ScalarType { override def toString: String = "nat" }

final case class IndexType(size: Nat) extends BasicType {
  override def toString: String = s"idx[$size]"
}

// TODO: enforce ScalarType
sealed case class VectorType(size: Nat, elemType: DataType) extends BasicType {
  override def toString: String = s"<$size>$elemType"
}

object vec {
  def apply(size: Nat, elemType: DataType) = VectorType(size, elemType)
}

final class NatToDataApply(val f: NatToData, val n: Nat) extends DataType {
  override def toString: String = s"$f($n)"
}

object NatToDataApply {
  def apply(f: NatToData, n: Nat): DataType = f match {
    case l: NatToDataLambda     => l.apply(n)
    case i: NatToDataIdentifier => new NatToDataApply(i, n)
  }

  def unapply(arg: NatToDataApply): Option[(NatToData, Nat)] =
    Some((arg.f, arg.n))
}

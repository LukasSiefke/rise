package idealised.utils

import idealised.DPIA.Nat

case class SizeInByte(value: Nat) {
  def *(rhs: Nat) = SizeInByte(value * rhs)
  def +(rhs: SizeInByte) = SizeInByte(value + rhs.value)

  override def toString = s"$value bytes"
}
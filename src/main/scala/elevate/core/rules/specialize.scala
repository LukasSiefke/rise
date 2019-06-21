package elevate.core.rules

import elevate.core.Strategy
import lift.core.primitives

object specialize {
  val mapSeq: Strategy = {
    case primitives.map => primitives.mapSeq
  }

  val reduceSeq: Strategy = {
    case primitives.reduce => primitives.reduceSeq
  }

  def slideSeq(rot: primitives.slideSeq.Rotate): Strategy = {
    case primitives.slide => primitives.slideSeq(rot)
  }
}
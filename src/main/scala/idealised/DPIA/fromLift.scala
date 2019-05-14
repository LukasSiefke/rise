package idealised.DPIA

import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA.Semantics.{OperationalSemantics => OpSem}
import idealised.OpenCL.FunctionalPrimitives.MapGlobal
import idealised.SurfaceLanguage.Operators
import lift.{core => l}
import lift.core.{types => lt}
import lift.core.{semantics => ls}

object fromLift {
  def apply(expr: l.Expr): Phrase[_ <: PhraseType] = {
    expr match {
      case l.TypedExpr(typedExpr, t) =>
        typedExpr match {
          case l.Identifier(name) =>
            Identifier(name, fromLift(t))

          case l.Lambda(x, e) => t match {
            case lt.FunctionType(i, _) =>
              Lambda(Identifier(x.name, fromLift(i)), fromLift(e))
            case _ => ???
          }
          case l.Apply(f, e) =>
            Apply(
              fromLift(f).asInstanceOf[Phrase[FunctionType[PhraseType, PhraseType]]],
              fromLift(e).asInstanceOf[Phrase[PhraseType]])

          case l.NatDepLambda(n, e) =>
            NatDependentLambda(n, fromLift(e))
          case l.NatDepApply(f, n) =>
            NatDependentApply(
              fromLift(f).asInstanceOf[Phrase[NatDependentFunctionType[PhraseType]]],
              n)

          case l.TypeDepLambda(dt, e) =>
            TypeDependentLambda(DataTypeIdentifier(dt.name), fromLift(e))
          case l.TypeDepApply(f, dt) =>
            TypeDependentApply(
              fromLift(f).asInstanceOf[Phrase[TypeDependentFunctionType[PhraseType]]],
              fromLift(dt)
            )

          case l.Literal(d)   =>  Literal(fromLift(d))
          case l.Index(n, sz) =>  Literal(OpSem.IndexData(n, IndexType(sz)))
          case l.NatExpr(n)   =>  Natural(n)
          case p: l.Primitive =>  fromLift(p, t)

          case _: l.TypedExpr => ??? // do not expect typed expr
        }
      case _ => ??? // expected typed expr
      }
  }

  def apply(t: lt.ScalarType): ScalarType = {
    t match {
      case lt.bool => bool
      case lt.int => int
      case lt.float => float
      case lt.double => double
      case lt.NatType => NatType
    }
  }

  def apply(t: lt.BasicType): BasicType = {
    t match {
      case st: lt.ScalarType => fromLift(st)
      case lt.IndexType(sz) => IndexType(sz)
      case lt.VectorType(sz, et) => et match {
        case e : lt.ScalarType => VectorType(sz, fromLift(e))
        case _ => ???
      }
    }
  }

  def apply(t: lt.DataType): DataType = {
    t match {
      case bt: lt.BasicType => fromLift(bt)
      case lt.DataTypeIdentifier(name) => DataTypeIdentifier(name)
      case lt.ArrayType(sz, et) => ArrayType(sz, fromLift(et))
      case lt.DepArrayType(sz, et) => ???
      case lt.TupleType(a, b) => RecordType(fromLift(a), fromLift(b))
    }
  }

  def apply(ty: lt.Type): PhraseType = {
    ty match {
      case dt: lt.DataType => ExpType(fromLift(dt))
      case lt.FunctionType(i, o) => FunctionType(fromLift(i), fromLift(o))
      case lt.TypeDependentFunctionType(dt, t) =>
        TypeDependentFunctionType(DataTypeIdentifier(dt.name), fromLift(t))
      case lt.NatDependentFunctionType(n, t) =>
        NatDependentFunctionType(n, fromLift(t))
    }
  }

  def apply(data: ls.Data): OpSem.Data = {
    data match {
      case ls.ArrayData(a) => OpSem.ArrayData(a.map(fromLift(_)).toVector)
      case ls.TupleData(a, b) => OpSem.RecordData(fromLift(a), fromLift(b))
      case ls.BoolData(b) => OpSem.BoolData(b)
      case ls.IntData(i) => OpSem.IntData(i)
      case ls.FloatData(f) => OpSem.FloatData(f)
      case ls.DoubleData(d) => OpSem.DoubleData(d)
      case ls.VectorData(v) => OpSem.VectorData(v.map(fromLift(_)).toVector)
    }
  }

  import lift.core.{primitives => core}
  import idealised.DPIA.FunctionalPrimitives._

  def fun[T <: PhraseType](t: T,
                           f: Phrase[T] => Phrase[_ <: PhraseType]): Phrase[_ <: PhraseType] = {
    val x = Identifier(freshName("x"), t)
    Lambda(x, f(x))
  }

  def apply(p: l.Primitive, t: lt.Type): Phrase[_ <: PhraseType] = {
    import lift.OpenCL.{primitives => ocl}
    import lift.OpenMP.{primitives => omp}
    import idealised.OpenMP.FunctionalPrimitives._

    (p, t) match {
      case (core.asIndex,
      lt.NatDependentFunctionType(n,
      lt.FunctionType(lt.NatType, lt.IndexType(_))))
      =>
        NatDependentLambda(n,
          fun[ExpType](exp"[$NatType]", e =>
            AsIndex(n, e)))

      case (core.map,
      lt.FunctionType(lt.FunctionType(_, lb: lt.DataType),
      lt.FunctionType(lt.ArrayType(n, la: lt.DataType), _)))
      =>
        makeMap(Map, n, la, lb)

      case (core.mapSeq,
      lt.FunctionType(lt.FunctionType(_, lb: lt.DataType),
      lt.FunctionType(lt.ArrayType(n, la: lt.DataType), _)))
      =>
        makeMap(MapSeq, n, la, lb)

      case (omp.mapPar,
      lt.FunctionType(lt.FunctionType(_, lb: lt.DataType),
      lt.FunctionType(lt.ArrayType(n, la: lt.DataType), _)))
      =>
        makeMap(MapPar, n, la, lb)

      case (ocl.mapGlobal(dim),
      lt.FunctionType(lt.FunctionType(_, lb: lt.DataType),
      lt.FunctionType(lt.ArrayType(n, la: lt.DataType), _)))
      =>
        makeMap(MapGlobal(dim), n, la, lb)

      case (core.depMapSeq,
      lt.FunctionType(
      lt.NatDependentFunctionType(k, lt.FunctionType(_, _)),
      lt.FunctionType(lt.DepArrayType(n, la), lt.DepArrayType(_, lb))))
      =>
        val a: NatDataTypeFunction = ??? // fromLift(la)
        val b: NatDataTypeFunction = ??? // fromLift(lb)
        fun[`(nat)->`[ExpType -> ExpType]](
          NatDependentFunctionType(k, ExpType(a(k)) -> ExpType(b(k))), f =>
          fun[ExpType](exp"[$n.$a]", e =>
            DepMapSeq(n, a, b, f, e)))


      case (core.reduceSeq,
      lt.FunctionType(_,
      lt.FunctionType(lb: lt.DataType,
      lt.FunctionType(lt.ArrayType(n, la), _))))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType -> (ExpType -> ExpType)](exp"[$a]" -> (exp"[$b]" -> exp"[$b]"), f =>
          fun[ExpType](exp"[$b]", i =>
            fun[ExpType](exp"[$n.$a]", e =>
              ReduceSeq(n, a, b, f, i, e))))

      case (core.scanSeq,
      lt.FunctionType(_,
      lt.FunctionType(lb: lt.DataType,
      lt.FunctionType(lt.ArrayType(n, la), _))))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType -> (ExpType -> ExpType)](exp"[$a]" -> (exp"[$b]" -> exp"[$b]"), f =>
          fun[ExpType](exp"[$b]", i =>
            fun[ExpType](exp"[$n.$a]", e =>
              ScanSeq(n, a, b, f, i, e))))

      case (core.depJoin,
        lt.FunctionType(lt.DepArrayType(n, llenF), lt.ArrayType(_, la)))
        =>
        val a = fromLift(la)
        val lenF: NatNatTypeFunction = ??? // fromLift(llenF)
        fun[ExpType](exp"[$n.${NatDataTypeFunction(n, (i:NatIdentifier) => ArrayType(lenF(i), a))}]", e =>
          DepJoin(n, lenF, a, e))

      case (core.join,
      lt.FunctionType(lt.ArrayType(n, lt.ArrayType(m, la)), _))
      =>
        val a = fromLift(la)
        fun[ExpType](exp"[$n.$m.$a]", e =>
          Join(n, m, a, e))

      case (core.split,
      lt.NatDependentFunctionType(n,
      lt.FunctionType(lt.ArrayType(insz, la), lt.ArrayType(m, _))))
      =>
        val a = fromLift(la)
        NatDependentLambda(n,
          fun[ExpType](exp"[$insz.$a]", e =>
            Split(n, m, a, e)))

      case (core.slide,
      lt.NatDependentFunctionType(sz,
      lt.NatDependentFunctionType(sp,
      lt.FunctionType(lt.ArrayType(insz, la), lt.ArrayType(n, _)))))
      =>
        val a = fromLift(la)
        NatDependentLambda(sz,
          NatDependentLambda(sp,
            fun[ExpType](exp"[$insz.$a]", e =>
              Slide(n, sz, sp, a, e))))

      case (core.slideSeq,
      lt.NatDependentFunctionType(sz,
      lt.NatDependentFunctionType(sp,
      lt.FunctionType(lt.ArrayType(insz, la), lt.ArrayType(n, _)))))
      =>
        val a = fromLift(la)
        NatDependentLambda(sz,
          NatDependentLambda(sp,
            fun[ExpType](exp"[$insz.$a]", e =>
              SlideSeq(n, sz, sp, a, e))))

      case (core.reorder,
      lt.FunctionType(_,
      lt.FunctionType(_,
      lt.FunctionType(lt.ArrayType(n, la), _))))
      =>
        val a = fromLift(la)
        fun[ExpType -> ExpType](exp"[idx($n)]" -> exp"[idx($n)]", idxF =>
          fun[ExpType -> ExpType](exp"[idx($n)]" -> exp"[idx($n)]", idxFinv =>
            fun[ExpType](exp"[$n.$a]", e =>
              Reorder(n, a, idxF, idxFinv, e))))

      case (core.transpose,
      lt.FunctionType(lt.ArrayType(n, lt.ArrayType(m, la)), _))
      =>
        val a = fromLift(la)

        val transposeFunction =
          λ(ExpType(IndexType(n * m)))(i => {
            mapIndexExpr(i, j => {
              val col = (j % n) * m
              val row = j / n
              row + col
            })
          })

        val transposeInverseFunction =
          λ(ExpType(IndexType(n * m)))(i => {
            mapIndexExpr(i, j => {
              val col = (j % m) * n
              val row = j / m
              row + col
            })
          })

        fun[ExpType](exp"[$n.$m.$a]", e =>
          Split(n, m, a,
            Reorder(n * m, a, transposeFunction, transposeInverseFunction,
              Join(n, m, a, e))))

      case (core.take,
      lt.NatDependentFunctionType(n,
      lt.FunctionType(lt.ArrayType(nm, la), _)))
      =>
        val m = nm - n
        val a = fromLift(la)
        NatDependentLambda(n,
          fun[ExpType](exp"[$nm.$a]", e =>
            Take(n, m, a, e)))

      case (core.drop,
      lt.NatDependentFunctionType(n,
      lt.FunctionType(lt.ArrayType(nm, la), _)))
      =>
        val m = nm - n
        val a = fromLift(la)
        NatDependentLambda(n,
          fun[ExpType](exp"[$nm.$a]", e =>
            Drop(n, m, a, e)))

      case (core.unzip,
      lt.FunctionType(
      lt.ArrayType(n, lt.TupleType(la, lb)),
      lt.TupleType(lt.ArrayType(_, _), lt.ArrayType(_, _))))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType](exp"[$n.($a x $b)]", e =>
            Unzip(n, a, b, e))

      case (core.zip,
      lt.FunctionType(lt.ArrayType(n, la),
      lt.FunctionType(lt.ArrayType(_, lb), _)))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType](exp"[$n.$a]", x =>
          fun[ExpType](exp"[$n.$b]", y =>
            Zip(n, a, b, x, y)))

      case (core.fst,
      lt.FunctionType(lt.TupleType(la, lb), _))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType](exp"[$a x $b]", e => Fst(a, b, e))

      case (core.snd,
      lt.FunctionType(lt.TupleType(la, lb), _))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType](exp"[$a x $b]", e => Snd(a, b, e))

      case (core.UnaryOp(op),
      lt.FunctionType(la: lt.DataType, _))
      =>
        val a = fromLift(la)
        fun[ExpType](exp"[$a]", e =>
          UnaryOp(uop(op), e))

      case (core.BinOp(op),
      lt.FunctionType(la: lt.DataType, _))
      =>
        val a = fromLift(la)
        fun[ExpType](exp"[$a]", x =>
          fun[ExpType](exp"[$a]", y =>
            BinOp(bop(op), x, y)))

      case (core.cast, lt.FunctionType(la: lt.BasicType, lb: lt.BasicType))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType](ExpType(a), x =>
          Cast(a, b, x))

      case (core.ForeignFunction(decl, la), _)
      =>
        val (inTs, outT) = foreignFunIO(la)
        wrapForeignFun(decl, inTs, outT, Vector())

      case (core.generate, lt.FunctionType(_, lt.ArrayType(n, la)))
      =>
        val a = fromLift(la)
        fun[ExpType -> ExpType](exp"[idx($n)]" -> ExpType(a), f =>
          Generate(n, a, f))

      case (core.iterate,
      lt.NatDependentFunctionType(k,
      lt.FunctionType(lt.NatDependentFunctionType(l,
      lt.FunctionType(lt.ArrayType(ln, _), _)),
      lt.FunctionType(lt.ArrayType(insz, _), lt.ArrayType(m, la)))))
      =>
        val n = ln /^ l
        val a = fromLift(la)
        NatDependentLambda(k,
          fun[`(nat)->`[ExpType -> ExpType]](
            NatDependentFunctionType(l, exp"[$ln.$a]" -> exp"[$l.$a]"), f =>
              fun[ExpType](exp"[$insz.$a]", e =>
                Iterate(n, m, k, a, f, e))))

      case (omp.asVector,
      lt.NatDependentFunctionType(n,
      lt.FunctionType(lt.ArrayType(mn, la: lt.ScalarType), lt.ArrayType(m, _))))
      =>
        val a = fromLift(la)
        NatDependentLambda(n,
          fun[ExpType](exp"[$mn.$a]", e =>
            AsVector(n, m, a, e)))

      case (omp.asScalar, lt.FunctionType(lt.ArrayType(m, lt.VectorType(n, la: lt.ScalarType)), _))
      =>
        val a = fromLift(la)
        fun[ExpType](ExpType(ArrayType(m, VectorType(n, a))), e =>
          AsScalar(m, n, a, e))

      case (omp.vectorFromScalar, lt.FunctionType(_, lt.VectorType(n, la: lt.ScalarType)))
      =>
        val a = fromLift(la)
        fun[ExpType](ExpType(a), e =>
          VectorFromScalar(n, a, e))

      case (core.indexAsNat, lt.FunctionType(lt.IndexType(n), lt.NatType))
      =>
        fun[ExpType](exp"[idx($n)]", e =>
          IndexAsNat(n, e))

      case (core.reduce, _) | (core.scan, _) =>
        throw new Exception(s"$p has no implementation")
    }
  }

  private def makeMap(map: (Nat, DataType, DataType, Phrase[ExpType -> ExpType], Phrase[ExpType]) => Phrase[_ <: PhraseType],
                      n: Nat,
                      la: lt.DataType,
                      lb: lt.DataType): Phrase[_ <: PhraseType] = {
    val a = fromLift(la)
    val b = fromLift(lb)
    fun[ExpType -> ExpType](ExpType(a) -> ExpType(b), f =>
      fun[ExpType](exp"[$n.$a]", e =>
        map(n, a, b, f, e)))
  }

  def foreignFunIO(t: lt.Type): (Vector[DataType], DataType) = {
    t match {
      case lt.FunctionType(laa, lb) => laa match {
        case la: lt.DataType =>
          val a = fromLift(la)
          val (i, o) = foreignFunIO(lb)
          (a +: i, o)
        case _ => ???
      }
      case lo: lt.DataType =>
        (Vector(), fromLift(lo))
    }
  }

  def wrapForeignFun(decl: core.ForeignFunctionDecl,
                     intTs: Vector[DataType],
                     outT: DataType,
                     args: Vector[Phrase[ExpType]]): Phrase[_ <: PhraseType] = {
    val i = args.length
    if (i < intTs.length) {
      fun[ExpType](ExpType(intTs(i)), a =>
        wrapForeignFun(decl, intTs, outT, args :+ a))
    } else {
      ForeignFunction(decl, intTs, outT, args)
    }
  }

  // TODO: remove surface language
  val Unary: Operators.Unary.type = idealised.SurfaceLanguage.Operators.Unary
  val Binary: Operators.Binary.type = idealised.SurfaceLanguage.Operators.Binary

  def uop(op: l.Operators.Unary.Value): Unary.Value = {
    import l.Operators.Unary._
    op match {
      case NEG => Unary.NEG
    }
  }

  def bop(op: l.Operators.Binary.Value): Binary.Value = {
    import l.Operators.Binary._
    op match {
      case ADD => Binary.ADD
      case SUB => Binary.SUB
      case MUL => Binary.MUL
      case DIV => Binary.DIV
      case MOD => Binary.MOD
      case GT => Binary.GT
      case LT => Binary.LT
      case EQ => Binary.EQ
    }
  }
}
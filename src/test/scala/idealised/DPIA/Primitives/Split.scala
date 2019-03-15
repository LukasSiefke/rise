package idealised.DPIA.Primitives

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker

class Split extends idealised.util.Tests {

  test("Simple split example should generate syntactic valid C code with two for loops") {
    val slideExample =
      nFun(n =>
        fun(ArrayType(n, float))(xs => xs :>> split(2) :>> mapSeq(mapSeq(fun(x => x)))))

    val p = idealised.C.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(slideExample, Map())))
    val code = p.code
    SyntaxChecker(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 2
  }

  test("Simple 2D split example with separate maps should generate syntactic valid OpenMP code with three for loops") {
    val slideExample =
      nFun(n => nFun(m =>
        fun(ArrayType(n, ArrayType(m, float)))(xs =>
          xs :>> map(split(2)) :>> mapSeq(mapSeq(mapSeq(fun(x => x)))) )))

    val p = idealised.OpenMP.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(slideExample, Map())))
    val code = p.code
    SyntaxChecker(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 3
  }

  test("Simple 2D split example with merged maps should generate syntactic valid OpenMP code with three for loops") {
    val slideExample =
      nFun(n => nFun(m =>
        fun(ArrayType(n, ArrayType(m, float)))( xs =>
          xs :>> mapSeq(split(2) >>> mapSeq(mapSeq(fun(x => x)))) )))

    val p = idealised.OpenMP.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(slideExample, Map())))
    val code = p.code
    SyntaxChecker(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 3
  }

  ignore ("Simple 2D dependent split should generated syntactic valid OpenCL code") {
    val splitExample =
      nFun(n =>
        fun(DepArrayType(n, i => ArrayType(i + 1, float)))(xs =>
          xs :>> split(4) :>> depMapSeq(fun(row => depMapSeq(fun(col => mapSeq(fun(x => x + 1.0f), col)), row)))
      ))

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(splitExample, Map())))
    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }

  ignore("Split mapAcc translation could work with functions working on independent elements") {
    val e = nFun(n => fun(ArrayType(n, float))(xs =>
      xs :>> mapSeq(fun(x => x)) :>> split(8) :>> map(fun(x => x))
    ))

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(e, Map())))
    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }
}

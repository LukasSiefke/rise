package apps

import lift.core._
import lift.core.DSL._
import lift.core.types._
import lift.OpenCL.primitives._

object nearestNeighbour {
  private val distance = foreignFun("distance_", Seq("loc", "lat", "lng"),
    "{ return sqrt((lat - loc._fst) * (lat - loc._fst) + (lng - loc._snd) * (lng -  loc._snd)); }",
    (float x float) ->: float ->: float ->: float)

  val nn: Expr = nFun(n => fun(
    (n`.`(float x float)) ->: float ->: float ->: (n`.`float)
  )((locations, lat, lng) =>
    locations |> mapGlobal(fun(loc => distance(loc)(lat)(lng)))
  ))

  import idealised.OpenCL._
  import util.{Time, TimeSpan}

  def runOriginalKernel(name: String,
                        locations: Array[Float],
                        lat: Float,
                        lng: Float): (Array[Float], TimeSpan[Time.ms]) = {
    import opencl.executor._

    val code = util.readFile(s"src/main/scala/apps/originalLift/$name")
    val kernelJNI = Kernel.create(code, "KERNEL", "")

    assert(locations.length % 2 == 0)
    val N = locations.length / 2
    val localSize = LocalSize(128)
    val globalSize = GlobalSize(N)

    val float_bytes = 4
    val output_bytes = N * float_bytes
    val g_out = GlobalArg.createOutput(output_bytes)
    val kernelArgs = Array(
      GlobalArg.createInput(locations),
      ValueArg.create(lat), ValueArg.create(lng),
      g_out,
      ValueArg.create(N)
    )

    val runtime = Executor.execute(kernelJNI,
      localSize.size.x.eval, localSize.size.y.eval, localSize.size.z.eval,
      globalSize.size.x.eval, globalSize.size.y.eval, globalSize.size.z.eval,
      kernelArgs
    )

    val output = g_out.asFloatArray()

    kernelArgs.foreach(_.dispose)
    kernelJNI.dispose()

    (output, TimeSpan.inMilliseconds(runtime))
  }

  def runKernel(k: KernelNoSizes,
                locations: Array[Float],
                lat: Float,
                lng: Float): (Array[Float], TimeSpan[Time.ms]) = {
    assert(locations.length % 2 == 0)
    val N = locations.length / 2
    val localSize = LocalSize(128)
    val globalSize = GlobalSize(N)

    val f = k.as[ScalaFunction `(`
      Int `,` Array[Float] `,` Float `,` Float
      `)=>` Array[Float]]
    f(localSize, globalSize)(N `,` locations `,` lat `,` lng)
  }

}
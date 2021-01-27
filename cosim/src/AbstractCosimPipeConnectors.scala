package cosim

import chisel3._
import chisel3.util._
import chiseltest._
import java.io._
import java.nio.file.{Files, Paths}
import verif._

abstract class AbstractCosimPipe extends Runnable {
  def run: Unit
  def exit: Unit
}

abstract class AbstractCosimPipeDriver[I, S, D](pipe: String) extends AbstractCosimPipe {
  @volatile private var terminate = false


  val driver: AbstractDriver[I, S]
  val inputStreamToProto: (java.io.InputStream) => D

  def pushIntoDriver(message: D): Unit

  override def run: Unit = {
    // Spin until all needed FIFOs are created
    while (!Files.exists(Paths.get(pipe))) {
      Thread.sleep(250)
    }

    println("All Pipe Driver files exist")

    val in = new FileInputStream(pipe)

    println("Pipe Driver streams opened")

    while(!terminate) {
      val message = inputStreamToProto(in)
      if (message != null) {
        pushIntoDriver(message)
      }
    }

    in.close
  }

  override def exit: Unit = {
    terminate = true
  }
}

abstract class AbstractCosimPipeMonitor[T <: com.google.protobuf.MessageLite](pipe: String){

  val out = new FileOutputStream(pipe)
}
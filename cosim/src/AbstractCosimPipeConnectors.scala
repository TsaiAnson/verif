package cosim

import chisel3._
import chisel3.util._
import chiseltest._
import java.io._
import verif._

abstract class AbstractCosimPipe extends Runnable {
  def run: Unit
  def exit: Unit
}

abstract class AbstractCosimPipeDriver[I, S, D](pipe: String) extends AbstractCosimPipe {
  @volatile private var terminate = false


  val driver: AbstractDriver[I, S]
  val inputStreamToLiteral: (java.io.InputStream) => D

  def pushIntoDriver(message: D): Unit

  override def run: Unit = {
    val in = new FileInputStream(pipe)

    while(!terminate) {
      val message = inputStreamToLiteral(in)

      pushIntoDriver(message)

      // We remove the double monitor driver pair because you cannot instantate a wire in a non-module
      // context
    }
  }

  override def exit: Unit = {
    terminate = true
  }
}

abstract class AbstractCosimPipeMonitor[T <: com.google.protobuf.MessageLite](pipe: String){

  val out = new FileOutputStream(pipe)
}
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

  val in = new FileInputStream(pipe)

  val io: Bundle
  val monitor: AbstractMonitor[I, S]
  val driver: AbstractDriver[I, S, D]
  val inputStreamToLiteral: (java.io.InputStream) => D

  def pokeIntoIO(message: D): Unit

  override def run: Unit = {
    while(!terminate) {
      val message = inputStreamToLiteral(in)

      pokeIntoIO(message)

      // This would be eliminated with a streaming driver -> monitor inferface
      // For now we need to be sure the monitored transaction is added to the driver before reading a new message
      var tx = monitor.getOldestMonitoredTransaction
      while (tx.isEmpty) { tx = monitor.getOldestMonitoredTransaction}
      driver.push(tx.get)
    }
  }

  override def exit: Unit = {
    terminate = true
  }
}

abstract class AbstractCosimPipeMonitor[T <: com.google.protobuf.MessageLite](pipe: String){

  val out = new FileOutputStream(pipe)
}
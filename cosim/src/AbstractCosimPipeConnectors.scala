package cosim

import java.io._
import verif._

abstract class AbstractCosimPipe extends Runnable {
  def run: Unit
  def stop: Unit
}

abstract class AbstractCosimPipeDriver[T <: Data](pipe: String, clock: Clock, gen: T) extends AbstractCosimPipe {
  @volatile private val terminate = false

  val in = new FileInputStream(pipe)
  val io = new ReadyValidIO(gen)
  val monitor = new ReadyValidMonitor[T](clock, io)

  val driver: AbstractDriver
  val inputStreamToLiteral: (java.io.InputStream) => T

  override def run: Unit = {
    io.bits.valid.poke(true.B)
    while(!terminate) {
      val message = inputStreamToLiteral(in)
      timescope {
        if (message.isInstanceOf[Bundle]) {
          io.bits.asInstanceOf[Bundle].pokePartial(t.data.asInstanceOf[Bundle])
        } else {
          io.bits.poke(message)
        }
      }

      // This would be eliminated with a streaming driver -> monitor inferface
      // For now we need to be sure the monitored transaction is added to the driver before reading a new message
      var tx = monitor.getOldestMonitoredTransaction
      while (tx.isEmpty) { tx = monitor.getOldestMonitoredTransaction}
      driver.push(tx.get)
    }
  }

  oerride def stop: Unit = {
    terminate = true
  }
}

abstract class AbstractCosimPipeMonitor[T <: com.google.protobuf.MessageLite](pipe: String){

  val out = new FileOutputStream(pipe)
}
package verif

import chisel3._
import chiseltest._
import freechips.rocketchip.tilelink._

import scala.collection.mutable.{HashMap, ListBuffer, MutableList, Queue}
import TLUtils._

// TLDriver acting as a Master node
class TLDriverMaster(clock: Clock, interface: TLBundle) {
  val params: TLBundleParameters = interface.params

  private val aDriver = new DecoupledDriverMaster(clock, interface.a)
  // TODO: dDriver and bDriver just consume transactions blindly
  private val dDriver = new DecoupledDriverSlave(clock, interface.d, 0)
  private val bDriver = if (interface.params.hasBCE) Option(new DecoupledDriverSlave(clock, interface.b, 0)) else None
  private val cDriver = if (interface.params.hasBCE) Option(new DecoupledDriverMaster(clock, interface.c)) else None
  private val eDriver = if (interface.params.hasBCE) Option(new DecoupledDriverMaster(clock, interface.e)) else None

  def push(tx: Seq[TLChannel]): Unit = {
    tx.foreach { channel: TLChannel =>
        assert(channel.isLit())
        channel match {
          case a: TLBundleA =>
            val txProto = DecoupledTX(new TLBundleA(params))
            val tx = txProto.tx(a)
            aDriver.push(tx)
          case c: TLBundleC =>
            val txProto = DecoupledTX(new TLBundleC(params))
            val tx = txProto.tx(c)
            cDriver.get.push(tx) // TODO: will throw exception is hasBCE = false
          case e: TLBundleE =>
            val txProto = DecoupledTX(new TLBundleE(params))
            val tx = txProto.tx(e)
            eDriver.get.push(tx)
          case _ =>
            throw new RuntimeException("TLDriverMaster got a TLBundleB or TLBundleD which can't be driven")
        }
    }
  }
}


// TLDriver acting as a Slave node
// Takes in a response function for processing requests
class TLDriverSlave[S](clock: Clock, interface: TLBundle, initState: S, response: (TLChannel, S) => (TLChannel, S)) {
  val txns = ListBuffer[TLChannel]()
  var state = initState

  // Responsible for collecting requests and calling on the user-defined response method
  // Currently only supports single source
  def process(a : TLBundleA) : Unit = {
    // Adding request to buffer (to collect for burst)
    txns += a

    // Checking if list buffer has complete TLTransaction
    if (isCompleteTLTxn(txns.toList)) {
      // Converting to TLTransaction
      val tltxn = TLBundlestoTLTransaction(txns.toList)
      txns.clear()

      // Calling on response function
      val tuple = response(tltxn, state)
      state = tuple._2

      // Converting back to TLChannels
      val responses = TLTransactiontoTLBundles(tuple._1)

      // Writing response(s)
      for (resp <- responses) {
        writeChannel(resp)

        // Clock step called here
        // We won't be getting any requests since A ready is low
        clk.step()
      }
    } else {
      // Step clock even if no response is driven (for burst requests)
      clk.step()
    }
  }

  // Currently just processes the requests from master
  fork {
    reset()
    while (true) {
      process(readA())
    }
  }
}

class TLMonitor(clock: Clock, interface: TLBundle) {
  val params = interface.params

  private val aMonitor = new DecoupledMonitor[TLChannel](clock, interface.a)
  private val dMonitor = new DecoupledMonitor[TLChannel](clock, interface.d)
  private val bMonitor = if (interface.params.hasBCE) Option(new DecoupledMonitor[TLChannel](clock, interface.b)) else None
  private val cMonitor = if (interface.params.hasBCE) Option(new DecoupledMonitor[TLChannel](clock, interface.c)) else None
  private val eMonitor = if (interface.params.hasBCE) Option(new DecoupledMonitor[TLChannel](clock, interface.e)) else None

  def getMonitoredTransactions(): Seq[DecoupledTX[TLChannel]] = {
    val tx = aMonitor.getMonitoredTransactions ++
      dMonitor.getMonitoredTransactions ++
      bMonitor.map(_.getMonitoredTransactions).getOrElse(Seq()) ++
      cMonitor.map(_.getMonitoredTransactions).getOrElse(Seq()) ++
      eMonitor.map(_.getMonitoredTransactions).getOrElse(Seq())
    tx.sortBy(_.cycleStamp.litValue())
  }
}

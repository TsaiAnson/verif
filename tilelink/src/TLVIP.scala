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
            val txProto = new DecoupledTX(new TLBundleA(params))
            val tx = txProto.tx(a)
            aDriver.push(tx)
          case c: TLBundleC =>
            val txProto = new DecoupledTX(new TLBundleC(params))
            val tx = txProto.tx(c)
            cDriver.get.push(tx) // TODO: will throw exception is hasBCE = false
          case e: TLBundleE =>
            val txProto = new DecoupledTX(new TLBundleE(params))
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
class TLDriverSlave[S](clock: Clock, interface: TLBundle, initState: S, response: (List[TLChannel], S) => (Seq[TLChannel], S)) {
  val params: TLBundleParameters = interface.params

  private val aMonitor = new DecoupledMonitor[TLChannel](clock, interface.a)
  private val dDriver = new DecoupledDriverMaster(clock, interface.d)
  // TODO DriverSlave currently DOES NOT support BCE
//  private val bDriver = if (interface.params.hasBCE) Option(new DecoupledDriverMaster(clock, interface.b)) else None
//  private val cDriver = if (interface.params.hasBCE) Option(new DecoupledDriverSlave(clock, interface.c, 0)) else None
//  private val eDriver = if (interface.params.hasBCE) Option(new DecoupledDriverSlave(clock, interface.e, 0)) else None

  val txns = ListBuffer[TLChannel]()
  var state = initState

  // Responsible for collecting requests and calling on the user-defined response method
  // Currently only supports single source
  def process(a : TLBundleA) : Unit = {
    // Adding request to buffer (to collect for burst)
    txns += a

    // Checking if list buffer has complete TLTransaction
    if (isCompleteTLTxn(txns.toList)) {
      // Calling on response function
      val tuple = response(txns.toList, state)
      txns.clear()
      state = tuple._2

      // Writing response(s)
      for (resp <- tuple._1) {
        val txProto = new DecoupledTX(new TLBundleD(params))
        val tx = txProto.tx(resp.asInstanceOf[TLBundleD])
        dDriver.push(tx)

        // Clock step called here
        // We won't be getting any requests since A ready is low
        clock.step()
      }
    } else {
      // Step clock even if no response is driven (for burst requests)
      clock.step()
    }
  }

  // Currently just processes the requests from master
  fork {
    while (true) {
      val decoupledTxn = aMonitor.getOldestMonitoredTransaction.getOrElse(None)
      if (decoupledTxn != None) {
        val data = decoupledTxn.asInstanceOf[DecoupledTX[TLChannel]].data
        process(data.asInstanceOf[TLBundleA])
      }
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

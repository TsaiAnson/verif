package verif

import chisel3._
import chiseltest._
import freechips.rocketchip.tilelink._

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
class TLDriverSlave[S](clock: Clock, interface: TLBundle, initState: S, response: (TLChannel, S, TLBundleParameters) => (Seq[TLChannel], S)) {
  val params: TLBundleParameters = interface.params

  // TODO: once DecoupledDriverSlave returns a stream of seen transactions, remove the monitor
  private val aDriver = new DecoupledDriverSlave[TLBundleA](clock, interface.a, 0)
  private val dDriver = new DecoupledDriverMaster[TLBundleD](clock, interface.d)
  private val bDriver = if (interface.params.hasBCE) Option(new DecoupledDriverMaster(clock, interface.b)) else None
  private val cDriver = if (interface.params.hasBCE) Option(new DecoupledDriverSlave(clock, interface.c, 0)) else None
  private val eDriver = if (interface.params.hasBCE) Option(new DecoupledDriverSlave(clock, interface.e, 0)) else None
  private val monitor = new TLMonitor(clock, interface)

  var state = initState

  fork {
    while (true) {
      // extract TLBundle A,C,E
      val txFromMaster = monitor.getMonitoredTransactions().map(_.data).flatMap{
        case _:TLBundleD | _:TLBundleB => None
        case other => Some(other)
      }
      val (responseTxns, newState) = txFromMaster.foldLeft((Seq.empty[TLChannel], state)) {
        case ((responses, state), tx) =>
          val (newTxns, newState) = response(tx, state, params)
          (responses ++ newTxns, newState)
      }
      state = newState
      dDriver.push(responseTxns.collect{ case t: TLBundleD => t }.map {
        t: TLBundleD =>
          new DecoupledTX(new TLBundleD(params)).tx(t)
      })
      // TODO: use map/foreach over bDriver instead of if-check
      if (params.hasBCE) {
        bDriver.get.push(responseTxns.collect{ case t: TLBundleB => t }.map {
          t: TLBundleB =>
            new DecoupledTX(new TLBundleB(params)).tx(t)
        })
      }
      clock.step()
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
    aMonitor.clearMonitoredTransactions()
    dMonitor.clearMonitoredTransactions()
    if (interface.params.hasBCE) {
      bMonitor.get.clearMonitoredTransactions()
      cMonitor.get.clearMonitoredTransactions()
      eMonitor.get.clearMonitoredTransactions()
    }
    tx.sortBy(_.cycleStamp.litValue())
  }
}

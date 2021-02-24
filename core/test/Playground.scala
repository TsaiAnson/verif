package verif

import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chisel3.util.Queue
import chisel3._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation

import scala.annotation.tailrec
import scala.collection.mutable

class Playground extends AnyFlatSpec with ChiselScalatestTester {
  def runCycle[I <: Record, T, S, E](io: I, vip: TestComponent[I,T,S,E], state: S): (S, Seq[E]) = {
    val toPoke = vip.getPokes(io.peek(), state)
    io.pokePartial(toPoke)
    val peek = io.peek()
    val newState = vip.update(peek, state)
    val emission = vip.emit(peek, newState)
    (newState, emission)
  }

  def runSim[I <: Record, T, S, E](clock: Clock, io: I, vip: TestComponent[I,T,S,E], initState: S): Seq[E] = {
    runSimRec(clock, io, vip, initState, Seq.empty[E])
  }

  @tailrec
  final def runSimRec[I <: Record, T, S, E](clock: Clock, io: I, vip: TestComponent[I,T,S,E], state: S, emitted: Seq[E]): Seq[E] = {
    if (!vip.busy(io.peek(), state)) {
      emitted
    } else {
      val (newState, emission) = runCycle(io, vip, state)
      clock.step()
      runSimRec(clock, io, vip, newState, emitted ++ emission)
    }
  }

  // One idea: compose 2 test components into a larger one that encompasses both interfaces!
  // then have a function that runs a test component over a circuit from an initial state and returns the emitted sequences
  // TODO: make stimulus a special test component type (takes things it needs and produces a new stim each cycle and can tell when it is finished)
  it should "run" in {
    val gen = UInt(8.W)
    val dut = () => new Queue(gen, 8, false, false)
    val master = new DecoupledMaster(gen)
    val slave = new DecoupledSlave(gen, new FixedBackpressure(2))
    val stim = Seq.tabulate(8)(i => new DecoupledTX[UInt](gen).tx((1+i).U, i, 0))
    var masterState = MasterState.stim[UInt](stim)
    var slaveState = SlaveState.empty()

    test(dut()).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      runSim(c.clock, c.io.enq, master, masterState)
      //do {
        //val (newMState, masterEmit) = runCycle(c.io.enq, master, masterState)
        //val (newSState, slaveEmit) = runCycle(c.io.deq, slave, slaveState)
        //masterState = newMState
        //slaveState = newSState
        /*
        masterState = master.newTxns(Seq.empty, masterState)
        slaveState = slave.newTxns(Seq.empty, slaveState)

        val toPokeMaster = master.getPokes(c.io.enq.peek(), masterState)
        val toPokeSlave = slave.getPokes(c.io.deq.peek(), slaveState)
        c.io.enq.pokePartial(toPokeMaster)
        c.io.deq.pokePartial(toPokeSlave)

        masterState = master.update(c.io.enq.peek(), masterState)
        slaveState = slave.update(c.io.deq.peek(), slaveState)

        master.emit(c.io.enq.peek(), masterState).foreach(println("Master", _))
        slave.emit(c.io.deq.peek(), slaveState).foreach(println("Slave", _))

         */

        //c.clock.step()
      //} while (master.busy(c.io.enq.peek(), masterState) || slave.busy(c.io.deq.peek(), slaveState))
    }
  }
}

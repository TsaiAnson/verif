// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

case class ShiftQueueIO[T <: Data](private val gen: T, val entries: Int) extends Bundle {
  val queueIO = new QueueIO(gen, entries)
  val enq = queueIO.enq
  val deq = queueIO.deq
  val count = queueIO.count
  val mask = Output(UInt(entries.W))
}

/** Implements the same interface as chisel3.util.Queue, but uses a shift
 * register internally.  It is less energy efficient whenever the queue
 * has more than one entry populated, but is faster on the dequeue side.
 * It is efficient for usually-empty flow-through queues. */
class ShiftQueue[T <: Data](gen: T,
                            val entries: Int,
                            pipe: Boolean = false,
                            flow: Boolean = false)
  extends Module {
  val io = IO(new ShiftQueueIO(gen, entries))

  private val valid = RegInit(VecInit(Seq.fill(entries) { false.B }))
  private val elts = Reg(Vec(entries, gen))

  for (i <- 0 until entries) {
    def paddedValid(i: Int) = if (i == -1) true.B else if (i == entries) false.B else valid(i)

    val wdata = if (i == entries-1) io.enq.bits else Mux(valid(i+1), elts(i+1), io.enq.bits)
    val wen =
      Mux(io.deq.ready,
        paddedValid(i+1) || io.enq.fire() && ((i == 0 && !flow).B || valid(i)),
        io.enq.fire() && paddedValid(i-1) && !valid(i))
    when (wen) { elts(i) := wdata }

    valid(i) :=
      Mux(io.deq.ready,
        paddedValid(i+1) || io.enq.fire() && ((i == 0 && !flow).B || valid(i)),
        io.enq.fire() && paddedValid(i-1) || valid(i))
  }

  io.enq.ready := !valid(entries-1)
  io.deq.valid := valid(0)
  io.deq.bits := elts.head

  if (flow) {
    when (io.enq.valid) { io.deq.valid := true.B }
    when (!valid(0)) { io.deq.bits := io.enq.bits }
  }

  if (pipe) {
    when (io.deq.ready) { io.enq.ready := true.B }
  }

  io.mask := valid.asUInt
  io.count := PopCount(io.mask)
}

object ShiftQueue
{
  def apply[T <: Data](enq: DecoupledIO[T], entries: Int = 2, pipe: Boolean = false, flow: Boolean = false): DecoupledIO[T] = {
    val q = Module(new ShiftQueue(enq.bits.cloneType, entries, pipe, flow))
    q.io.enq <> enq
    q.io.deq
  }
}

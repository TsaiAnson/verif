package verif

import scala.math.pow
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleParameters, TLChannel}
import verif.TLTransaction._

class TLMemoryModel(p: TLBundleParameters) extends TLSlaveFunction[TLMemoryModel.State] {
  override def response(tx: TLChannel, state: TLMemoryModel.State) : (Seq[TLChannel], TLMemoryModel.State) = {
    val bytesPerWord = p.dataBits/8
    implicit val params: TLBundleParameters = p
    tx match {
      case txA: TLBundleA =>
        txA.opcode.litValue().toInt match {
          case TLOpcodes.Get =>
            val byteAddr = txA.address.litValue()
            assert(byteAddr % bytesPerWord == 0)
            val wordAddr = (byteAddr / bytesPerWord).toLong
            val wordsToSend = (pow(2, txA.size.litValue().toInt) / bytesPerWord).toInt
            val responseTxs = (0 until wordsToSend).map {
              wordIdx => if (state.mem.contains(wordAddr+wordIdx)) BigInt(state.mem(wordAddr + wordIdx)) else BigInt(0)
            }.map {
              word => AccessAckData(word, 0, txA.size.litValue().toInt, txA.source.litValue().toInt)
            }
            (responseTxs, state)
          case TLOpcodes.PutPartialData | TLOpcodes.PutFullData =>
            val writeData = txA.data.litValue()
            val writeMask = txA.mask.litValue().toInt
            // TODO duplicated, also handle bursts
            val byteAddr = txA.address.litValue()
            assert(byteAddr % bytesPerWord == 0)
            val wordAddr = (byteAddr / bytesPerWord).toLong
            val newMem = TLMemoryModel.write(state.mem, wordAddr, writeData, writeMask)
            (Seq(AccessAck(0)), state.copy(mem = newMem))

            // TODO: handle logic and arithmetic + bursts + masks
          case _ => ???
        }
      case _ => ???
    }
  }
}
// TODO apply method with with varargs of Map for initializing
object TLMemoryModel {
  type WordAddr = Long
  case class State (
                     mem: Map[WordAddr, Array[Byte]]
                   )
  object State {
    def empty() = State(Map[WordAddr, Array[Byte]]())
    def init(mem: Map[WordAddr, BigInt]): State = State(mem.mapValues(_.toByteArray))
  }

  def write(mem: Map[WordAddr, Array[Byte]], wordAddr: WordAddr, data: BigInt, mask: Int)(implicit p: TLBundleParameters): Map[WordAddr, Array[Byte]] = {
    val bytesPerWord = p.dataBits/8
    // BigInt.toByteArray returns a big endian byte representation
    // Need to left pad dataBytes to bytesPerWord
    val dataBytes = data.toByteArray.reverse.padTo(bytesPerWord, 0.toByte).reverse
    //dataBytes.foreach(b => println(f"$b%x"))
    assert(dataBytes.length == bytesPerWord)
    val bytesToWrite = (0 until bytesPerWord).collect{
      case i if ((mask >> i) & 0x1) == 1 => i
    }.map(i => bytesPerWord - i - 1) // convert mask to big endian
    val initialValue = if (mem.contains(wordAddr)) mem(wordAddr) else Array.fill(bytesPerWord)(0.toByte)
    var newValue = initialValue
    for (byteIdx <- bytesToWrite) {
      newValue = newValue.updated(byteIdx, dataBytes(byteIdx))
    }
    mem + (wordAddr -> newValue)
  }
}

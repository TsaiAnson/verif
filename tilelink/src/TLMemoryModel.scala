package verif

import scala.math.pow
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleParameters, TLChannel}
import verif.TLTransaction._

class TLMemoryModel(p: TLBundleParameters) extends TLSlaveFunction[TLMemoryModel.State] {
  implicit val params: TLBundleParameters = p

  override def response(tx: TLChannel, state: TLMemoryModel.State) : (Seq[TLChannel], TLMemoryModel.State) = {
    val bytesPerWord = p.dataBits/8

    tx match {
      case txA: TLBundleA =>
        val byteAddr = txA.address.litValue()
        assert(byteAddr % bytesPerWord == 0)
        val wordAddr = (byteAddr / bytesPerWord).toLong
        val wordsToProcess = (pow(2, txA.size.litValue().toInt) / bytesPerWord).toInt

        txA.opcode.litValue().toInt match {
          case TLOpcodes.Get =>
            val responseTxs = (0 until wordsToProcess).map {
              wordIdx => TLMemoryModel.read(state.mem, wordAddr + wordIdx, txA.mask.litValue().toInt, bytesPerWord)
                //if (state.mem.contains(wordAddr+wordIdx)) BigInt(Array(0.toByte) ++ state.mem(wordAddr+wordIdx)) else BigInt(0)
            }.map {
              word => AccessAckData(word, 0, txA.size.litValue().toInt, txA.source.litValue().toInt)
            }
            (responseTxs, state)
          case TLOpcodes.PutPartialData | TLOpcodes.PutFullData =>
            val writeData = txA.data.litValue()
            val writeMask = txA.mask.litValue().toInt
            // We're currently in a write burst
            if (state.burstStatus.isDefined) {
              val burstStatus = state.burstStatus.get
              val newMem = TLMemoryModel.write(state.mem, burstStatus.baseAddr + burstStatus.currentBeat, writeData, writeMask, bytesPerWord)
              val newBurstStatus = if ((burstStatus.currentBeat + 1) == burstStatus.totalBeats) {
                None
              } else {
                Some(burstStatus.copy(currentBeat = burstStatus.currentBeat + 1))
              }
              (Seq.empty[TLChannel], state.copy(mem = newMem, burstStatus = newBurstStatus))
            } else {
              val newMem = TLMemoryModel.write(state.mem, wordAddr, writeData, writeMask, bytesPerWord)
              if (wordsToProcess == 1) { // Single beat write
                (Seq(AccessAck(0, txA.source.litValue().toInt)), state.copy(mem = newMem))
              } else { // Starting a burst
                val burstStatus = TLMemoryModel.BurstStatus(wordAddr, 1, wordsToProcess)
                (Seq(AccessAck(0, txA.size.litValue().toInt, txA.source.litValue().toInt)), state.copy(mem = newMem, burstStatus = Some(burstStatus)))
              }
            }
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
                     mem: Map[WordAddr, Array[Byte]],
                     burstStatus: Option[BurstStatus]
                   )
  case class BurstStatus(baseAddr: WordAddr, currentBeat: Int, totalBeats: Int)

  object State {
    def empty(): State = State(Map[WordAddr, Array[Byte]](), None)
    def init(mem: Map[WordAddr, BigInt]): State = State(mem.mapValues(_.toByteArray), None)
  }

  private def maskToBigEndian(mask: Int, bytesPerWord: Int): Seq[Int] = {
    (0 until bytesPerWord).collect {
      case i if ((mask >> i) & 0x1) == 1 => i
    }.map(i => bytesPerWord - i - 1)
  }

  def write(mem: Map[WordAddr, Array[Byte]], wordAddr: WordAddr, data: BigInt, mask: Int, bytesPerWord: Int): Map[WordAddr, Array[Byte]] = {
    // BigInt.toByteArray returns a big endian byte representation
    // Need to left pad dataBytes to bytesPerWord
    val dataBytes = data.toByteArray.reverse.padTo(bytesPerWord, 0.toByte).reverse

    // 0xffffffff = Array[Byte](0, -1, -1, -1, -1)
    assert(dataBytes.length == bytesPerWord || (dataBytes.head == 0.toByte && dataBytes.length == bytesPerWord + 1))
    val dataBytesCleaned = if (dataBytes.length == bytesPerWord) dataBytes else dataBytes.slice(1, dataBytes.length)

    val bytesToWrite = maskToBigEndian(mask, bytesPerWord)
    val initialValue = if (mem.contains(wordAddr)) mem(wordAddr) else Array.fill(bytesPerWord)(0.toByte)
    var newValue = initialValue
    for (byteIdx <- bytesToWrite) {
      newValue = newValue.updated(byteIdx, dataBytesCleaned(byteIdx))
    }
    mem + (wordAddr -> newValue)
  }

  def read(mem: Map[WordAddr, Array[Byte]], wordAddr: WordAddr, mask: Int, bytesPerWord: Int): BigInt = {
    // In TileLink, Get requests with a mask only read the active byte lanes and the other lanes can contain any data
    // So it is OK to naively read from the memory and return the full word
    // However, we choose to zero out the bytes that are low in the mask to potentially uncover issues in other software models
    val bytesToRead = maskToBigEndian(mask, bytesPerWord)
    val readBytes = if (mem.contains(wordAddr)) mem(wordAddr) else Array.fill(bytesPerWord)(0.toByte)
    val readBytesMasked = readBytes.zipWithIndex.foldLeft(Seq.empty[Byte]) {
      case (bytesToReturn, (readByte, idx)) =>
        bytesToReturn :+ (if (bytesToRead.contains(idx)) readByte else 0.toByte)
    }
    BigInt(Array(0.toByte) ++ readBytesMasked)
  }
}

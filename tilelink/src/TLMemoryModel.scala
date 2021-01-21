package verif

import scala.math.{ceil, pow}
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
        val wordsToProcess = ceil(pow(2, txA.size.litValue().toInt) / bytesPerWord).toInt

        txA.opcode.litValue().toInt match {
          case TLOpcodes.Get =>
            val responseTxs = (0 until wordsToProcess).map {
              wordIdx => TLMemoryModel.read(state.mem, wordAddr + wordIdx, txA.mask.litValue().toInt, bytesPerWord)
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
          case TLOpcodes.LogicalData | TLOpcodes.ArithmeticData => // TODO: support logic/arith bursts
            val writeMask = txA.mask.litValue().toInt
            // We're currently in a write burst
            if (state.burstStatus.isDefined) {
              val burstStatus = state.burstStatus.get
              val readData = TLMemoryModel.read(state.mem, burstStatus.baseAddr + burstStatus.currentBeat, txA.mask.litValue().toInt, bytesPerWord)
              val writeData = TLMemoryModel.dataToWrite(readData, txA.data.litValue(), txA.opcode.litValue().toInt, txA.param.litValue().toInt)
              val newMem = TLMemoryModel.write(state.mem, burstStatus.baseAddr + burstStatus.currentBeat, writeData, writeMask, bytesPerWord)
              val newBurstStatus = if ((burstStatus.currentBeat + 1) == burstStatus.totalBeats) {
                None
              } else {
                Some(burstStatus.copy(currentBeat = burstStatus.currentBeat + 1))
              }
              (Seq(AccessAckData(readData, 0, txA.size.litValue().toInt, txA.source.litValue().toInt)), state.copy(mem = newMem, burstStatus = newBurstStatus))
            } else {
              val readData = TLMemoryModel.read(state.mem, wordAddr, txA.mask.litValue().toInt, bytesPerWord)
              val writeData = TLMemoryModel.dataToWrite(readData, txA.data.litValue(), txA.opcode.litValue().toInt, txA.param.litValue().toInt)
              val newMem = TLMemoryModel.write(state.mem, wordAddr, writeData, writeMask, bytesPerWord)
              if (wordsToProcess == 1) { // Single beat read-modify-write
                (Seq(AccessAckData(readData, txA.source.litValue().toInt)), state.copy(mem = newMem))
              } else { // Starting a burst
                val burstStatus = TLMemoryModel.BurstStatus(wordAddr, 1, wordsToProcess)
                (Seq(AccessAckData(readData, 0, txA.size.litValue().toInt, txA.source.litValue().toInt)), state.copy(mem = newMem, burstStatus = Some(burstStatus)))
              }
            }
          case _ => ???
        }
      case _ => ???
    }
  }
}

object TLMemoryModel {
  type WordAddr = Long
  case class BurstStatus(baseAddr: WordAddr, currentBeat: Int, totalBeats: Int)
  case class State(mem: Map[WordAddr, Array[Byte]], burstStatus: Option[BurstStatus]) {
    // Convert Array[Byte] into hex string
    override def toString: String = {
      mem.map{
        case (addr, word) => f"$addr -> ${BigInt(Array(0.toByte) ++ word)}%#x"
      }.mkString("\n") + "\n" +
      burstStatus.toString + "\n"
    }
  }

  object State {
    def empty(): State = State(Map[WordAddr, Array[Byte]](), None)
    def init(mem: Map[WordAddr, BigInt], bytesPerWord: Int): State =
      State(mem.mapValues(v => padBigEndian(v, bytesPerWord)), None)
  }

  private def maskToBigEndian(mask: Int, bytesPerWord: Int): Seq[Int] = {
    (0 until bytesPerWord).collect {
      case i if ((mask >> i) & 0x1) == 1 => i
    }.map(i => bytesPerWord - i - 1)
  }

  // BigInt.toByteArray returns a big endian byte representation
  // Need to left pad dataBytes to bytesPerWord
  private def padBigEndian(num: BigInt, padToBytes: Int): Array[Byte] = {
    val bytes = num.toByteArray.reverse.padTo(padToBytes, 0.toByte).reverse
    // 0xffffffff = Array[Byte](0, -1, -1, -1, -1)
    assert(bytes.length == padToBytes|| (bytes.head == 0.toByte && bytes.length == padToBytes + 1))
    if (bytes.length == padToBytes) bytes else bytes.slice(1, bytes.length)
  }

  def write(mem: Map[WordAddr, Array[Byte]], wordAddr: WordAddr, data: BigInt, mask: Int, bytesPerWord: Int): Map[WordAddr, Array[Byte]] = {
    val dataBytes = padBigEndian(data, bytesPerWord)
    val bytesToWrite = maskToBigEndian(mask, bytesPerWord)
    val initialValue = if (mem.contains(wordAddr)) mem(wordAddr) else Array.fill(bytesPerWord)(0.toByte)
    var newValue = initialValue
    for (byteIdx <- bytesToWrite) {
      newValue = newValue.updated(byteIdx, dataBytes(byteIdx))
    }
    mem + (wordAddr -> newValue)
  }

  def read(mem: Map[WordAddr, Array[Byte]], wordAddr: WordAddr, mask: Int, bytesPerWord: Int): BigInt = {
    // In TileLink, Get requests with a mask only read the active byte lanes and the other lanes can contain any data
    // So it is OK to naively read from the memory and return the full word
    // However, we choose to zero out the bytes that are low in the mask to potentially uncover issues in other software models
    val bytesToRead = maskToBigEndian(mask, bytesPerWord)
    val readBytes = if (mem.contains(wordAddr)) mem(wordAddr) else Array.fill(bytesPerWord)(0.toByte)
    assert(readBytes.length == bytesPerWord)
    val readBytesMasked = readBytes.zipWithIndex.foldLeft(Seq.empty[Byte]) {
      case (bytesToReturn, (readByte, idx)) =>
        bytesToReturn :+ (if (bytesToRead.contains(idx)) readByte else 0.toByte)
    }
    BigInt(Array(0.toByte) ++ readBytesMasked)
  }

  def dataToWrite(readData: BigInt, writeData: BigInt, opcode: Int, param: Int): BigInt = {
    opcode match {
      case TLOpcodes.LogicalData =>
        param match {
          case 0 => readData ^ writeData
          case 1 => readData | writeData
          case 2 => readData & writeData
          case 3 => writeData
          case _ => ???
        }
      case TLOpcodes.ArithmeticData =>
        param match {
          case 0 => readData.min(writeData) // TODO: these are forced unsigned min/max since BigInt is created from Chisel UInt, should be signed
          case 1 => readData.max(writeData)
          case 2 => readData.min(writeData)
          case 3 => readData.max(writeData)
          case 4 => readData + writeData
          case _ => ???
        }
    }
  }
}

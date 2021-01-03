package designs

import chisel3._

case class CAMIO(keyWidth: Int, dataWidth: Int) extends Bundle {
  val en = Input(Bool())
  val we = Input(Bool())
  val keyRe = Input(UInt(keyWidth.W))
  val keyWr = Input(UInt(keyWidth.W))
  val dataWr = Input(UInt(dataWidth.W))
  val found = Output(Bool())
  val dataRe = Output(UInt(dataWidth.W))
}

class ParameterizedCAMAssociative(keyWidth: Int, dataWidth: Int, memSizeWidth: Int) extends MultiIOModule {
  require(keyWidth >= 0)
  require(dataWidth >= 0)
  require(memSizeWidth >= 0)
  val io = IO(new CAMIO(keyWidth, dataWidth))

  // Defining our "Memory"
  val memorySize = math.pow(2, memSizeWidth).toInt
  val keyReg = RegInit(VecInit(Seq.fill(memorySize)(0.U(keyWidth.W))))
  val valReg = RegInit(VecInit(Seq.fill(memorySize)(0.U(dataWidth.W))))

  // Combinational Logic
  val wrIndex  = RegInit(0.U(memSizeWidth.W))
  val index = Wire(UInt(memSizeWidth.W))
  val found = (keyReg(index) === io.keyRe) && io.en

  when (io.we) {
    keyReg(wrIndex) := io.keyWr
    valReg(wrIndex) := io.dataWr
    wrIndex := wrIndex + 1.U
  }

  when (io.en) {
    index := keyReg.indexWhere(_ === io.keyRe)
  } .otherwise {
    index := (memorySize - 1).U
  }

  // Outputs
  io.found := found
  io.dataRe := valReg(index)
}

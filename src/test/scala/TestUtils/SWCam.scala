package verif

import designs.CAMIO
import chisel3._
import chisel3.experimental.BundleLiterals._

class SWAssocCAM (keyWidth: Int, dataWidth: Int, memSizeWidth: Int) {
  // var valid = new Array[Boolean](memSizeWidth)
  implicit val randGen: VerifRandomGenerator = new ScalaVerifRandomGenerator
  val keys = new Array[Int](scala.math.pow(2,memSizeWidth).toInt)
  val values = new Array[Int](scala.math.pow(2,memSizeWidth).toInt)
  var writeIndex = 0
  var found = false
  var dataRe = values.last
  val protoTx = CAMIO(keyWidth, dataWidth)

  def process (input: CAMIO) : CAMIO = {
    // Processing Writes First
    if (input.we.litToBoolean) {
      // valid(writeIndex) = true
      keys(writeIndex) = input.keyWr.litValue().toInt
      values(writeIndex) = input.dataWr.litValue().toInt
      writeIndex = if (writeIndex == (memSizeWidth - 1)) 0 else (writeIndex + 1)
    }

    // Processing Reads
    var temp_index = -1
    if (input.en.litToBoolean) {
      temp_index = keys.indexOf(input.keyRe.litValue())
    }

    // if (temp_index >= 0 && valid(temp_index)) {
    found = false
    dataRe = values.last
    if (temp_index >= 0) {
      found = true
      dataRe = values(temp_index)
    }

    protoTx.Lit(_.en -> input.en, _.we -> input.we, _.keyRe -> input.keyRe, _.keyWr -> input.keyWr,
      _.dataWr -> input.dataWr, _.found ->found.B, _.dataRe -> dataRe.U)
  }
}

// object Main {
//   def main(args: Array[String]): Unit = {
//     var swcam = new SWAssocCAM(8,8,8)
//     var out = swcam.process(CAMIOInTr(true, false, 123, 0, 0))
//     println(out.found)
//     out = swcam.process(CAMIOInTr(true, false, 0, 0, 0))
//     println(out.found)
//     out = swcam.process(CAMIOInTr(true, true, 0, 123, 123))
//     println(out.found)
//     out = swcam.process(CAMIOInTr(true, true, 0, 456, 789))
//     println(out.found)
//     out = swcam.process(CAMIOInTr(true, true, 0, 0, 7809))
//     println(out.found)
//   }
// }

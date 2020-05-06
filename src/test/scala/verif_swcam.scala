package verif

class SWAssocCAM (keyWidth: Int, dataWidth: Int, memSizeWidth: Int) {
  // var valid = new Array[Boolean](memSizeWidth)
  val keys = new Array[Int](scala.math.pow(2,memSizeWidth).toInt)
  val values = new Array[Int](scala.math.pow(2,memSizeWidth).toInt)
  var writeIndex = 0
  var found = false
  var dataRe = values.last

  def process (input: CAMIOInTr) : CAMIOOutTr = {
  	// Handling Null Transaction
  	if (input.isInstanceOf[CAMIOInTrNull]) {
  		return CAMIOOutTr(found, dataRe)
  	}

    // Processing Writes First
    if (input.we) {
      // valid(writeIndex) = true
      keys(writeIndex) = input.keyWr
      values(writeIndex) = input.dataWr
      writeIndex = if (writeIndex == (memSizeWidth - 1)) 0 else (writeIndex + 1)
    }

    // Processing Reads
    var temp_index = -1
    if (input.en) {
      temp_index = keys.indexOf(input.keyRe)
    }

    // if (temp_index >= 0 && valid(temp_index)) {
    found = false
    dataRe = values.last
    if (temp_index >= 0) {
      found = true
      dataRe = values(temp_index)
    }

    CAMIOOutTr(found, dataRe)
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

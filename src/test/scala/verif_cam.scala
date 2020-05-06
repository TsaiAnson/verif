package verif

import chisel3._

case class CAMIOInTr(en:Boolean, we:Boolean, keyRe: Int, keyWr: Int, dataWr: Int) extends Transaction

class CAMIOInTrNull() extends CAMIOInTr(false, false, 0, 0, 0)

case class CAMIOOutTr(found:Boolean, dataRe:Int) extends Transaction

package verif

import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import TLTransaction._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.subsystem.WithoutTLMonitors
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleParameters}
import org.scalatest.flatspec.AnyFlatSpec

class TLPropertyTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = new WithoutTLMonitors
  implicit val params: TLBundleParameters = TLUtils.defaultVerifTLBundleParams

  it should "test self property" in {
    def zeroParamGet(t: TLBundleA): Boolean = {
      if (t.opcode.litValue() == TLOpcodes.Get) {
        t.param.litValue() == 0
      } else {
        true
      }
    }
    val getProperty = TLSelfPropertyA(zeroParamGet)

    val inputTransactions = Seq(
      // Read back the values in registers 0x00, 0x08, 0x10, 0x18
      Get(0x0),
      Get(0x08),
      Get(0x10),
      Get(0x18),
      // Write values into registers 0x00, 0x08, 0x10, 0x18
      Put(0x0, 0),
      Put(0x8, 1),
      Put(0x10, 2),
      Put(0x18, 3),
      // Read back the values in registers 0x00, 0x08, 0x10, 0x18
      Get(0x0),
      Get(0x08),
      Get(0x10),
      Get(0x18)
    )
    assert(getProperty.check(inputTransactions).foldLeft(true)(_ & _))

    val inputTransactionsBad = Seq(
      // Read back the values in registers 0x00, 0x08, 0x10, 0x18
      Get(0x0),
      Get(0x08),
      Get(0x10),
      new TLBundleA(params).Lit(_.opcode -> TLOpcodes.Get.U, _.param -> 1.U, _.size -> 3.U,
        _.source -> 0.U, _.address -> 0x0.U, _.mask -> 0xff.U, _.corrupt -> 0.B, _.data -> 0.U),
      // Write values into registers 0x00, 0x08, 0x10, 0x18
      Put(0x0, 0),
      Put(0x8, 1),
      Put(0x10, 2),
      Put(0x18, 3),
      // Read back the values in registers 0x00, 0x08, 0x10, 0x18
      Get(0x0),
      Get(0x08),
      Get(0x10),
      Get(0x18)
    )
    assert(!getProperty.check(inputTransactionsBad).foldLeft(true)(_ & _))
  }
}
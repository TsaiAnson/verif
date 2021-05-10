package cosim

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, TreadleBackendAnnotation, WriteVcdAnnotation}
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.tile.{RoCCCommand}
import freechips.rocketchip.tilelink.{TLBundleA}
import freechips.rocketchip.diplomacy.{TransferSizes}
import org.scalatest.flatspec.AnyFlatSpec
import verif._

import com.verif._

class ProtoTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = VerifTestUtils.getVerifParameters()

  it should "Protobuf Test" in {
    val cmd = RoCCProtos.RoCCCommand.newBuilder()
      .setRs2(((BigInt(2) << 48) + (BigInt(1) << 32)).toLong)
      .setInst(
        RoCCProtos.RoCCInstruction.newBuilder()
          .setFunct(2))
      .build()

    val cmd_bundle = VerifProtoBufUtils.ProtoToBundle(cmd, VerifBundleUtils, new RoCCCommand)
    val cmd_roundtrip = VerifProtoBufUtils.BundleToProto(cmd_bundle, RoCCProtos.RoCCCommand.newBuilder())

    println("--- PROTO MESSAGE [RoCCCommand] ---")
    println(cmd)
    println("--- CHISEL BUNDLE [RoCCCommand] ---")
    println(cmd_bundle)
    println("--- ROUND TRIP PROTO [RoCCCommand] ---")
    println(cmd_roundtrip)


    val tla = TLProtos.TLA.newBuilder()
      .setData("0F0E0D0C0B0A09080706050403020100")
      .build()


    val tla_bundle = VerifProtoBufUtils.ProtoToBundle(tla, VerifBundleUtils, new TLBundleA(VerifTestUtils.getVerifTLBundleParameters(16, 32, TransferSizes(1,64))))
    val tla_roundtrip = VerifProtoBufUtils.BundleToProto(tla_bundle, TLProtos.TLA.newBuilder())

    println("--- PROTO MESSAGE [TLA] ---")
    println(tla)
    println("--- CHISEL BUNDLE [TLA] ---")
    println(tla_bundle)
    println("--- ROUND TRIP PROTO [TLA] ---")
    println(tla_roundtrip)

    assert(true)
  }
}

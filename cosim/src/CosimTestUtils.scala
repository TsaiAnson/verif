//******************************************************************************
// Copyright (c) 2018 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
package cosim

import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.{sanitizeFileName}
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.ClockSinkParameters
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import java.io.{ByteArrayOutputStream, PrintWriter}
import org.scalatest._
import org.scalatest.TestSuite
import reflect.runtime._
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.reflect.ClassTag
import scala.reflect.io.File
import scala.sys.process._
import universe._

import com.google.protobuf.Descriptors.FieldDescriptor.Type._

class CosimTestDetails {
  var sbtRoot: Option[String] = None
  var testName: Option[String] = None
  def testPath: Option[String] = if (sbtRoot.nonEmpty && testName.nonEmpty) Some(sbtRoot.get + "/test_run_dir/" + sanitizeFileName(testName.get)) else None
}

trait CosimTester extends TestSuiteMixin with BeforeAndAfterAllConfigMap { this: TestSuite =>
  var simTarget = ""

  implicit val cosimTestDetails = new CosimTestDetails

  override def beforeAll(configMap: ConfigMap) = {
    if (configMap.get("simTarget").isDefined) {
      simTarget = configMap.get("simTarget").fold("")(_.toString)
    }
  }

  abstract override def withFixture(test: NoArgTest): Outcome = {
    cosimTestDetails.sbtRoot = Some(s"${File(".").toAbsolute}")
    cosimTestDetails.testName = Some(test.name)
    super.withFixture(test)
  }
}

object VerifProtoBufUtils {
  def getHelper[T: TypeTag](obj: T, target: String) = typeOf[T]
    .decls
    .filter(x => x.fullName contains target)
    .head
    .asMethod

  def getArgs(method: MethodSymbol) = method
    .paramLists
    .flatten

  // Zip used because does not preserve ordering
  def getArgsZip(args: List[Symbol]) = args.map(x => x.name.toString)
    .zip(args.map(x => x.typeSignature))

  /**
   * WIP: Generic Proto to Bundle function.
   * Requires that every field set in the proto has a exact match (by field name) in a constructor defined as [BundleName]Helper within
   * some object pass in as helpers. Note the the BundleName and the ProtoName must also match. Finally all non-primitive items must be defined (i.e.
   * you cannot leave the RoCCInstruction within a RoCCCommand entirely undefined. All primitive undefined values are set to 0.
   *
   * As more bundle types are added more conversions within the case statements and default values will need to be defined.
   *
   * Note: The proto's set of fields may be a superset of the bundle's set of fields.
   */
  def ProtoToBundle[R <: Bundle, H <: Object](proto: com.google.protobuf.Message, helpers: H, returnType: R)
                                (implicit p: Parameters, helperType: TypeTag[H], helperClass: ClassTag[H]): R = {

    // Get the helper method that matches this item type
    val protoName = proto.getDescriptorForType.getName
    val helper = getHelper(helpers, protoName)

    ProtoToBundle(proto, helper, helpers, helpers, returnType)
  }

  private def ProtoToBundle[R, T: TypeTag, C: ClassTag](proto: com.google.protobuf.Message, helper: MethodSymbol,
                                                helperType: T, helperClass: C, returnType: R)
                                (implicit p: Parameters): R = {

    // Extract fields from the protobuf
    val protoArgs = collection.mutable.Map[String, Any]()
    proto.getAllFields.foreach { case (field, value) =>
      val fieldName = field.getName
      field.getType match {
        case MESSAGE =>
          // Recursive case needs some extra spice
          val subProto = value.asInstanceOf[com.google.protobuf.Message]
          val subProtoName = subProto.getDescriptorForType.getName
          val subHelper = getHelper(helperType, subProtoName)
          protoArgs += (fieldName -> ProtoToBundle(subProto, subHelper, helperType, helperClass, subHelper.returnType))
        case UINT32 => protoArgs += (fieldName -> fromBigIntToLiteral((BigInt(value.asInstanceOf[Integer] >>> 1) << 1) + (value.asInstanceOf[Integer] & 1)).asUInt)
        case UINT64 => protoArgs += (fieldName -> fromBigIntToLiteral((BigInt(value.asInstanceOf[Long] >>> 1) << 1) + (value.asInstanceOf[Long] & 1)).asUInt)
        case BOOL => protoArgs += (fieldName -> fromBooleanToLiteral(value.asInstanceOf[Boolean]).asBool)
        case STRING => protoArgs += (fieldName -> fromBigIntToLiteral(BigInt(value.asInstanceOf[String], 16)).asUInt)
      }
    }

    // Get a zip of argument -> type for our helper function.
    // Fill undeclared values with 0s and set the implicit parameters to p
    val args = getArgsZip(getArgs(helper))
      .map { case (arg, typ) =>
        protoArgs.getOrElse(arg, // Match arguments list to the values from the proto if present, else get a default
          typ match {
            case t if t =:= typeOf[chisel3.UInt] => 0.U
            case t if t =:= typeOf[chisel3.Bool] => false.B
            case t if t =:= typeOf[freechips.rocketchip.config.Parameters] => p
          })
      }

    currentMirror
      .reflect(helperClass)
      .reflectMethod(helper)
      .apply(args.toList: _*)
      .asInstanceOf[R]
  }

  def BundleToProto[B <: Bundle](bundle: B,
                                 builder: com.google.protobuf.Message.Builder): com.google.protobuf.Message = {
    com.google.protobuf.util.JsonFormat.parser().ignoringUnknownFields().merge("{" + BundleToJson(bundle) + "}", builder)
    builder.build()
  }

  def BundleToJson[B <: Bundle](bundle: B): String = {
    bundle.elements.map { case (name, value) =>
      value match {
        case _: Bundle => s""" "${name}": { ${BundleToJson(value.asInstanceOf[Bundle])} }"""
        case _: Bool => s""""${name}": ${value.asInstanceOf[Bool].litToBoolean}"""
        case _: UInt => value.asInstanceOf[UInt].getWidth match { // Values wider than 64.W are stored as hex strings
          case x if x > 64 => s""""${name}": "${String.format(s"%${math.ceil(x.toDouble / 4).toInt}s", value.litValue.toString(16).toUpperCase).replace(" ", "0")}""""
          case _ => s""""${name}": ${value.litValue}"""
        }
        case _ => s""""${name}": ${value.litValue}"""
      }
    }.mkString(",\n")
  }
}

object VerifBundleUtils {
  def RoCCCommandHelper(inst: RoCCInstruction = new RoCCInstruction, rs1: UInt = 0.U, rs2: UInt = 0.U)
                       (implicit p: Parameters): RoCCCommand = {
    new RoCCCommand().Lit(_.inst -> inst, _.rs1 -> rs1, _.rs2 -> rs2, _.status -> MStatusHelper(dprv = 3.U, prv = 3.U))
  }

  def RoCCInstructionHelper(funct: UInt = 0.U, rs2: UInt = 0.U, rs1: UInt = 0.U, xd: Bool = false.B,
                            xs1: Bool = false.B, xs2: Bool = false.B, rd: UInt = 0.U,
                            opcode: UInt = 0.U): RoCCInstruction = {
    new RoCCInstruction().Lit(_.funct -> funct, _.rs2 -> rs2, _.rs1 -> rs1, _.xd -> xd, _.xs1 -> xs1,
      _.xs2 -> xs2, _.rd -> rd, _.opcode -> opcode)
  }

  def MStatusHelper(debug: Bool = false.B, cease: Bool = false.B, wfi: Bool = false.B, isa: UInt = 0.U,
                    dprv: UInt = 0.U, prv: UInt = 0.U, sd: Bool = false.B, zero2: UInt = 0.U, sxl: UInt = 0.U,
                    uxl: UInt = 0.U, sd_rv32: Bool = false.B, zero1: UInt = 0.U, tsr: Bool = false.B,
                    tw: Bool = false.B, tvm: Bool = false.B, mxr: Bool = false.B, sum: Bool = false.B,
                    mprv: Bool = false.B, xs: UInt = 0.U, fs: UInt = 0.U, mpp: UInt = 0.U, vs: UInt = 0.U,
                    spp: UInt = 0.U, mpie: Bool = false.B, hpie: Bool = false.B, spie: Bool = false.B,
                    upie: Bool = false.B, mie: Bool = false.B, hie: Bool = false.B, sie: Bool = false.B,
                    uie: Bool = false.B)(implicit p: Parameters): MStatus = {
    new MStatus().Lit(_.debug -> debug, _.cease -> cease, _.wfi -> wfi, _.isa -> isa, _.dprv -> dprv, _.prv -> prv,
      _.sd -> sd, _.zero2 -> zero2, _.sxl -> sxl, _.uxl -> uxl, _.sd_rv32 -> sd_rv32, _.zero1 -> zero1, _.tsr -> tsr,
      _.tw -> tw, _.tvm -> tvm, _.mxr -> mxr, _.sum -> sum, _.mprv -> mprv, _.xs -> xs, _.fs -> fs, _.mpp -> mpp,
      _.vs -> vs, _.spp -> spp, _.mpie -> mpie, _.hpie -> hpie, _.spie -> spie, _.upie -> upie, _.mie -> mie,
      _.hie -> hie, _.sie -> sie, _.uie -> uie)
  }

  def TLAHelper(opcode: UInt = 0.U, param: UInt = 0.U, size: UInt = 3.U(3.W), source: UInt = 1.U, address: UInt = 0.U,
                mask: UInt = 0xff.U, data: UInt = 0.U, corrupt: Bool = false.B) : TLBundleA = {
    new TLBundleA(VerifTestUtils.getVerifTLBundleParameters(16, 32, TransferSizes(1,64))).Lit(_.opcode -> opcode, _.param -> param, _.size -> size, _.source -> source,
      _.address -> address, _.mask -> mask, _.data -> data, _.corrupt -> corrupt)
  }

  def TLDHelper (opcode: UInt = 0.U, param: UInt = 0.U, size: UInt = 3.U(3.W), source: UInt = 1.U, sink: UInt = 0.U,
                 data: UInt = 0.U, denied: Bool = false.B, corrupt: Bool = false.B) : TLBundleD = {
    new TLBundleD(VerifTestUtils.getVerifTLBundleParameters(16, 32, TransferSizes(1,64))).Lit(_.opcode -> opcode, _.param -> param, _.size -> size, _.source -> source,
      _.sink -> sink, _.data -> data, _.denied -> denied, _.corrupt -> corrupt)
  }
}

/**
 * Dummy tile params for out of context elaboration
 */
case object VerifTileParams extends TileParams {
  val name: Option[String] = Some("verif_tile")
  val hartId: Int = 0
  val core: RocketCoreParams = RocketCoreParams()
  val clockSinkParams: ClockSinkParameters = ClockSinkParameters()
  val beuAddr: Option[BigInt] = None
  val blockerCtrlAddr: Option[BigInt] = None
  val btb: Option[BTBParams] = None
  val dcache: Option[DCacheParams] = Some(DCacheParams(rowBits=128)) // TODO: can these be derived from beat bytes, etc
  val icache: Option[ICacheParams] = Some(ICacheParams(rowBits=128))
}

class VerifBaseRocketConfig extends Config(
  new freechips.rocketchip.subsystem.WithNBigCores(1) ++         // single rocket-core
  new freechips.rocketchip.subsystem.WithJtagDTM ++              // set the debug module to expose a JTAG port
  new freechips.rocketchip.subsystem.WithNoMMIOPort ++           // no top-level MMIO master port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithNoSlavePort ++          // no top-level MMIO slave port (overrides default set in rocketchip)
  //new freechips.rocketchip.subsystem.WithInclusiveCache ++       // use Sifive L2 cache TODO: Why doesn't this work
  new freechips.rocketchip.subsystem.WithoutTLMonitors ++         // single rocket-core
  new freechips.rocketchip.subsystem.WithNExtTopInterrupts(0) ++ // no external interrupts
  new freechips.rocketchip.system.BaseConfig)                    // "base" rocketchip system

/**
 * Factory object to help create a set of Verif parameters to use in tests
 */
object VerifTestUtils {
  def getVerifTLMasterPortParameters(): TLMasterPortParameters = {
    TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("bundleBridgeToTL")))
  }

  def getVerifTLSlavePortParameters(beatBytes: Int = 16, pAddrBits: Int = 32,
                                    transferSize: TransferSizes = TransferSizes(1, 64)): TLSlavePortParameters = {
    TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(address = Seq(AddressSet(0x0, BigInt("1"*pAddrBits, 2))),
      supportsGet = transferSize, supportsPutFull = transferSize, supportsPutPartial = transferSize)), beatBytes)
  }

  def getVerifTLBundleParameters(beatBytes: Int = 16, pAddrBits: Int = 32,
                             transferSize: TransferSizes = TransferSizes(1, 64)): TLBundleParameters = {
    TLBundleParameters(getVerifTLMasterPortParameters(), getVerifTLSlavePortParameters(beatBytes, pAddrBits, transferSize)).copy(sourceBits = 5) //TOD: This is a hack, need to add some way to specify source bits in the bundle creaion process
  }

  def getVerifParameters(
      xLen: Int = 64,
      beatBytes: Int = 16,
      blockBytes: Int = 64,
      pAddrBits: Int = 32,
      transferSize: TransferSizes = TransferSizes(1, 64)): Parameters = {

    val origParams = new VerifBaseRocketConfig

    // augment the parameters
    implicit val p = origParams.alterPartial {
      case MonitorsEnabled => false
      case TileKey => VerifTileParams
      case XLen => xLen // Have to use xLen to avoid errors with naming
      case PgLevels => if (xLen == 64) 3 else 2
      case MaxHartIdBits => 1
      case SystemBusKey => SystemBusParams(
        beatBytes = beatBytes,
        blockBytes = blockBytes
      )
    }

    // TODO: should these be args to the main function as well for ease of use?
    def verifTLUBundleParams: TLBundleParameters = TLBundleParameters(addressBits = 64, dataBits = 64, sourceBits = 1,
      sinkBits = 1, sizeBits = 6,
      echoFields = Seq(), requestFields = Seq(), responseFields = Seq(),
      hasBCE = false)

    val dummyInNode = BundleBridgeSource(() => TLBundle(verifTLUBundleParams))
    val dummyOutNode = BundleBridgeSink[TLBundle]()

    val tlMasterXbar = LazyModule(new TLXbar)
    val visibilityNode = TLEphemeralNode()(ValName("tile_master"))

    visibilityNode :=* tlMasterXbar.node
    tlMasterXbar.node :=
      BundleBridgeToTL(getVerifTLMasterPortParameters) :=
      dummyInNode

    dummyOutNode :=
      TLToBundleBridge(getVerifTLSlavePortParameters(beatBytes, pAddrBits, transferSize)) :=
      visibilityNode

    val outParams = p.alterPartial {
      case TileVisibilityNodeKey => visibilityNode
    }

    outParams
  }
}

object VerifCosimTestUtils {
  def runCommand(cmd: Seq[String]): Int = {
    Process(cmd, None, System.getenv().asScala.toSeq:_*).!
  }
}

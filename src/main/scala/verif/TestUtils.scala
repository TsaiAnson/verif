//******************************************************************************
// Copyright (c) 2018 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package verif

import org.scalatest._

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._

import chipyard.{RocketConfig}

import freechips.rocketchip.config._
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import reflect.runtime._
import scala.collection.JavaConversions._
import scala.collection.mutable.{ListBuffer}
import universe._
import com.google.protobuf.Descriptors.FieldDescriptor.Type._

import com.verif._

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
  def getArgsZip(args: List[Symbol]) = args.map(x => x.name.toString())
    .zip(args.map(x => x.typeSignature))

  def RoCCInstructionProtoToBundle(proto: com.google.protobuf.Message): RoCCInstruction = {
    return VerifBundleUtils.RoCCInstructionHelper()
  }

  def RoCCCommandProtoToBundle(proto: com.google.protobuf.Message)(implicit p: Parameters): RoCCCommand = {
    return VerifBundleUtils.RoCCCommandHelper()
  }

  /**
   * WIP: Generic Proto to Bundle function.
   * Requires that ever field set in the proto has a exact match (by field name) in a constructor defined as <BundleName>Helper within
   * VerifBundleUtils. Note the the BundleName and the ProtoName must also match. Finally all non-primitive items must be defined (i.e.
   * you cannot leave the RoCCInstruction within a RoCCCommand entirely undefined. All primitive undefined values are set to 0.
   *
   * As more bundle types are added more conversions within the case statements and default values will need to be defined
   */
  def ProtoToBundle[T](proto: com.google.protobuf.Message, returnType: T)
                                (implicit p: Parameters): T = {

    // Get the helper method that matches this item type
    val protoName = proto.getDescriptorForType().getName()
    val helper = getHelper(VerifBundleUtils, protoName)

    return ProtoToBundle(proto, helper, returnType)
  }

  def ProtoToBundle[T](proto: com.google.protobuf.Message, helper: MethodSymbol, returnType: T)
                                (implicit p: Parameters): T = {

    // Extract fields from the protobuf
    val protoArgs = collection.mutable.Map[String, Any]()
    proto.getAllFields().foreach(kv => {
      val (field, value) = kv
      val fieldName = field.getName()
      field.getType() match {
        case MESSAGE => {
          // Recursive case needs some extra spice
          val subProto = value.asInstanceOf[com.google.protobuf.Message]
          val subProtoName = subProto.getDescriptorForType().getName()
          val subHelper = getHelper(VerifBundleUtils, subProtoName)
          protoArgs += (fieldName -> ProtoToBundle(subProto, subHelper, subHelper.returnType))
        }
        case UINT32 => {
          protoArgs += (fieldName -> fromIntToLiteral(value.asInstanceOf[Integer]).asUInt)
        }
        case UINT64 => {
          protoArgs += (fieldName -> fromLongToLiteral(value.asInstanceOf[Long]).asUInt)
        }
        case BOOL => {
          protoArgs += (fieldName -> fromBooleanToLiteral(value.asInstanceOf[Boolean]).asBool)
        }
      }
    })

    // Get a zip of argument -> type for our helper function.
    // Fill undeclared values with 0s and set the implicit parameters to p
    val args = getArgsZip(getArgs(helper))
      .map(tuple => {
        val (arg, typ) = tuple
        if (protoArgs.contains(arg)) {
          protoArgs.get(arg).get
        } else {
          typ match {
            case t if t =:= typeOf[chisel3.UInt] => 0.U
            case t if t =:= typeOf[chisel3.Bool] => false.B
            case t if t =:= typeOf[freechips.rocketchip.config.Parameters] => p
          }
        }
      })

    return currentMirror
      .reflect(VerifBundleUtils)
      .reflectMethod(helper)
      .apply(args.toList: _*)
      .asInstanceOf[T]
  }
}

object VerifBundleUtils {
  def RoCCCommandHelper(inst: RoCCInstruction = new RoCCInstruction, rs1: UInt = 0.U, rs2: UInt = 0.U)
                       (implicit p: Parameters): RoCCCommand = {
    new RoCCCommand().Lit(_.inst -> inst, _.rs1 -> rs1, _.rs2 -> rs2)
  }

  def RoCCInstructionHelper(funct: UInt = 0.U, rs2: UInt = 0.U, rs1: UInt = 0.U, xd: Bool = false.B,
                            xs1: Bool = false.B, xs2: Bool = false.B, rd: UInt = 0.U,
                            opcode: UInt = 0.U): RoCCInstruction = {
    new RoCCInstruction().Lit(_.funct -> funct, _.rs2 -> rs2, _.rs1 -> rs1, _.xd -> xd, _.xs1 -> xs1,
      _.xs2 -> xs2, _.rd -> rd, _.opcode -> opcode)
  }
}

/**
 * Dummy tile params for out of context elaboration
 */
case object VerifTileParams extends TileParams {
  val name: Option[String] = Some("verif_tile")
  val hartId: Int = 0
  val core: RocketCoreParams = RocketCoreParams()
  val beuAddr: Option[BigInt] = None
  val blockerCtrlAddr: Option[BigInt] = None
  val btb: Option[BTBParams] = None
  val dcache: Option[DCacheParams] = Some(DCacheParams())
  val icache: Option[ICacheParams] = Some(ICacheParams())
}

/**
 * Factory object to help create a set of Verif parameters to use in tests
 */
object VerifTestUtils {
  def getVerifParameters(
      xLen: Int = 64,
      beatBytes: Int = 16,
      blockBytes: Int = 64,
      pAddrBits: Int = 32,
      transferSize: TransferSizes = TransferSizes(1, 64)): Parameters = {

    val origParams = new RocketConfig
    //val origParams = Parameters.empty

    // augment the parameters
    implicit val p = origParams.alterPartial {
      case MonitorsEnabled => false
      case TileKey => VerifTileParams
      case XLen => xLen // Have to use xLen to avoid errors with naming
      case PgLevels => if (xLen == 64) 3 else 2
      case MaxHartIdBits => 1
      case SystemBusKey => SystemBusParams(
        beatBytes = beatBytes,
        blockBytes = blockBytes // Is 64 the right value?
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
      BundleBridgeToTL(TLClientPortParameters(Seq(TLClientParameters("bundleBridgeToTL")))) :=
      dummyInNode
    //TODO: maybe values here paramterized
    //NOTE: address mask needed to be 0xffffffff so that paddrBits was 32 and not 12 (mask 0xfff)
    dummyOutNode :=
      TLToBundleBridge(TLManagerPortParameters(Seq(TLManagerParameters(address = Seq(AddressSet(0x0, BigInt("1"*pAddrBits, 2))),
        supportsGet = transferSize, supportsPutFull = transferSize)), beatBytes)):=
      visibilityNode

    val outParams = p.alterPartial {
      case TileVisibilityNodeKey => visibilityNode
    }

    //orgParams
    outParams
  }
}

package verif

import chisel3._
import chisel3.experimental.{Analog, EnumType, FixedPoint, Interval}
import chisel3.util.MixedVec

/**
 *  Enables literal comparisons between Chisel Bundle literals
 */
trait Transaction extends IgnoreSeqInBundle { this: Bundle =>
  // TODO: Should be a special equality mixin to ignore certain Bundle fields
  override def equals(that: Any): Boolean = {
    val classEquality = this.getClass == that.getClass
    if (!classEquality) return false; // TODO: is this too strict?
    val myElements = this.getElements
    val theirElements = that.asInstanceOf[Bundle].getElements
    if (myElements.length != theirElements.length) return false; // TODO: is this necessary?
    myElements.zip(theirElements).forall {
      case (myElem, theirElem) =>
        myElem match {
          case aggregate: Aggregate => aggregate match {
            case vec: Vec[_] => vec.zip(theirElem.asInstanceOf[Vec[_]]).forall {
              case (myVecElem, theirVecElem) =>
                myVecElem == theirVecElem // TODO: if vector elements are Bits and not Aggregate, equality check should be explicit
            }
            case record: Record => record match {
              case bundle: Bundle => bundle == theirElem.asInstanceOf[Bundle] // Recursive, assumes the nested Bundle also mixes in Transaction
              case mixedvec: MixedVec[_] => ???
              case _ => ???
            }
          }
          case element: Element => element match {
            case _:Clock | _:Reset | _:Analog | _:AsyncReset | _:EnumType | _:ResetType => ???
            case bits: Bits =>
              bits match {
                case uint: UInt => uint.isLit() && theirElem.asInstanceOf[UInt].isLit() &&
                  uint.litValue() == theirElem.asInstanceOf[UInt].litValue()
                case sint: SInt => sint.isLit() && theirElem.asInstanceOf[SInt].isLit() &&
                  sint.litValue() == theirElem.asInstanceOf[SInt].litValue()
                case fp: FixedPoint => ???
                case interval: Interval => ???
              }
          }
          case _ => ???
        }
    }
  }
}

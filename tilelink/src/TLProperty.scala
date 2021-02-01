package verif

import freechips.rocketchip.tilelink._
import verif.TLUtils._
import chisel3._
import chisel3.util.log2Ceil
import freechips.rocketchip.diplomacy.TransferSizes
import scala.collection.mutable
import scala.collection.immutable
import TLTransaction._

// Same cycle, same object parameter check
class TLSelfProperty[T](property: T => Boolean) {
  def check(input: T): Boolean = {
    property(input)
  }

  def check(input: Seq[T]): Seq[Boolean] = {
    input.map(property(_))
  }
}
/*
Notes: Properties should only return false if the condition is true and property is false (cannot do opcode == Get && \
  param == 0, as it will fail on non-gets)
 */

case class TLSelfPropertyA(property: TLBundleA => Boolean) extends TLSelfProperty[TLBundleA](property)
case class TLSelfPropertyD(property: TLBundleD => Boolean) extends TLSelfProperty[TLBundleD](property)
// Can continue for other TLChannel types BCE

//// Future cycles (lax), nonconsecutive
//// Won't need a strict (consecutive) one for TL, but will need for general case
//// TODO Handle repetitions and/or overlapping
//class TLFutureProperty[T](init_property: T => Boolean, future_property: T => Boolean) {
//  def check(input: T): Boolean = {
//    property(input)
//  }
//
//  def check(input: Seq[T], count: Int): Boolean = {
//
//  }
//}

// Thoughts: Can return dangling bundles?
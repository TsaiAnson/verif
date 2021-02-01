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
// Can continue for other TLChannel types BCE, and generic type (TLChannel)

// Future cycles (lax), nonconsecutive
// Won't need a strict (consecutive) one for TL, but will need for general case
// TODO Handle repetitions? Overlapping? Init properties that are same as future property (e.g. burst)
// TODO Add way to have dependent properties (e.g. if size is burst, future property checks if constant fields match)
class TLFutureProperty[T](init_property: T => Boolean, future_property: T => Boolean, count: Int) {
  var remaining_count = 0
  def checkInit(input: T): Unit = {
    if (init_property(input)) remaining_count += count
  }

  def checkFuture(input: T): Unit = {
    if (future_property(input)) remaining_count -= 1
  }

  // Cannot handle overlapping/concurrent properties (e.g. Get(source = 0) -> Get(source = 1) -> AccessAckData(1) -> AccessAckData(0))
  // Note: Currently assumes complete transaction sequence
  def check(input: Seq[T]): Boolean = {
    for (i <- input) {if (remaining_count == 0) checkInit(i) else checkFuture(i)}
    remaining_count == 0
  }
}

case class TLFuturePropertyA(property1: TLBundleA => Boolean, property2: TLBundleA => Boolean, count: Int = 1) extends
  TLFutureProperty[TLBundleA](property1, property2, count)
case class TLFuturePropertyD(property1: TLBundleD => Boolean, property2: TLBundleD => Boolean, count: Int = 1) extends
  TLFutureProperty[TLBundleD](property1, property2, count)
case class TLFuturePropertyAD(property1: TLChannel => Boolean, property2: TLChannel => Boolean, count: Int = 1) extends
  TLFutureProperty[TLChannel](property1, property2, count)

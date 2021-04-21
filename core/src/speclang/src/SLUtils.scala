package verif

import scala.collection.mutable.HashMap
import sourcecode.Name

package object SL {
  // Quick TimeOperation
  def ###(cycles: Int): TimeOp = ###(cycles, cycles)
  def ###(lower: Int, upper: Int): TimeOp = new TimeOp(lower, upper)

  // Quick Implication
  def Implies: Implication = new Implication

  // Quick Atomic Property
  def qAP[T,H,M](proposition: (T, HashMap[String, H], Option[SLMemoryState[M]]) => Boolean, desc: String = "Default")(implicit name: Name): AtmProp[T,H,M]
    = new AtmProp[T,H,M](proposition, if (desc == "Default") name.value else desc)

  // Quick Sequence
  def qSeq[T,H,M](input: SequenceElement*): Sequence[T,H,M] = new Sequence[T,H,M](input:_*)

  // Quick Property
  def qProp[T,H,M](input: Sequence[T,H,M], desc: String = "Default")(implicit name: Name): Property[T,H,M] =
    new Property[T,H,M](input, if (desc == "Default") name.value else desc)
}
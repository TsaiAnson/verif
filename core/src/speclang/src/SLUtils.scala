package verif

import scala.collection.mutable.HashMap

package object SL {
  // Quick TimeOperation
  def ###(cycles: Int): TimeOp = ###(cycles, cycles)

  def ###(start: Int, end: Int): TimeOp = {
    if (start == -1) new TimeOp(end, modifier = 2) // At most
    else if (end == -1) new TimeOp(start, modifier = 1) // At least
    else if (start == end) new TimeOp(start, modifier = 0) // Exactly
    else new TimeOp(start, end, 3) // Between
  }

  // Quick Implication
  def Implies: Implies = new Implies

  // Quick Atomic Property
  def qAP[T,H,M](proposition: (T, HashMap[String, H], Option[SLMemoryState[M]]) => Boolean, desc: String = "Default")(implicit name: sourcecode.Name): AtmProp[T,H,M]
    = new AtmProp[T,H,M](proposition, if (desc == "Default") name.value else desc)

  // Quick Sequence
  def qSeq[T,H,M](input: SequenceElement*): Sequence[T,H,M] = new Sequence[T,H,M](input:_*)

  // Quick Property
  def qProp[T,H,M](input: Sequence[T,H,M], desc: String = "Default")(implicit name: sourcecode.Name): Property[T,H,M] =
    new Property[T,H,M](input, if (desc == "Default") name.value else desc)
}
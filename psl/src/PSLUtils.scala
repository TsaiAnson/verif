package verif

import scala.collection.mutable.HashMap

package object PSL {
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
  def qAP[T,H,M](proposition: (T, HashMap[String, H], Option[PSLMemoryState[M]]) => Boolean, desc: String): AtmProp[T,H,M]
    = new AtmProp[T,H,M](proposition, desc)

  // Quick Property
  def qProp[T,H,M](input: SequenceElement*): Property[T,H,M] = new Property[T,H,M](new Sequence[T,H,M](input:_*))
  def qProp[T,H,M](input: Sequence[T,H,M]): Property[T,H,M] = new Property[T,H,M](input)
}
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
  def qAP[T,  H](proposition: (T, HashMap[String, H]) => Boolean, desc: String): AtmProp[T,  H] = new AtmProp[T,  H](proposition, desc)

  // Quick Property
  def qProp[T,  H](input: SequenceElement*): Property[T,  H] = new Property[T,  H](new Sequence[T,  H](input:_*))
}
package verif

import chisel3._

/**
 *  Enables literal comparisons between Chisel Bundle literals
 */
trait Transaction extends IgnoreSeqInBundle { this: Bundle =>
  override def equals(that: Any): Boolean = {
    var result = this.getClass() == that.getClass()
    if (result) {
      val fields = this.getClass.getDeclaredFields
      for (field <- fields) {
        field.setAccessible(true)
        if (field.get(this).isInstanceOf[List[UInt]]) {
          result &= field.get(this).asInstanceOf[List[UInt]].map((x: UInt) => x.litValue()).sameElements(
            field.get(that).asInstanceOf[List[UInt]].map((x: UInt) => x.litValue()))
        } else {
          result &= field.get(this).asInstanceOf[Data].litValue() == field.get(that).asInstanceOf[Data].litValue()
        }
      }
    }
    result
  }

  override def toString(): String = {
    var result = this.className + "("

    val fields = this.getClass.getDeclaredFields
    for (field <- fields) {
      field.setAccessible(true)
      if (field.get(this).isInstanceOf[List[UInt]]) {
        result += field.getName + ": ("
        for (u <- field.get(this).asInstanceOf[List[UInt]]) {
          result += u.litValue().toString() + ", "
        }
        result = result.slice(0, result.length - 2) + "), "
      } else {
        result += field.getName + ": "+ field.get(this).asInstanceOf[Data].litValue().toString() + ", "
      }
    }
    result = result.slice(0, result.length - 2) + ")"

    result
  }
}

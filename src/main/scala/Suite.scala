class Suite(c: Char) {
  val s = c

  val strength =  s match {
    case 'H' => 1
    case 'D' => 2
    case 'C' => 3
    case 'S' => 4
  }

  override def toString = s.toString
  override def hashCode: Int = strength.hashCode()
  override def equals(other: Any): Boolean =
    other match {
      case o: Suite =>
        (o canEqual this) &&
          s == o.s
      case _ => false
    }

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[Suite]

}

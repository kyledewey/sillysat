package sillysat.syntax

sealed trait Literal {
  def negated: Literal
  def v: Variable
  def isNegated: Boolean
}
case class PositiveAtom(v: Variable) extends Literal {
  def negated = NegativeAtom(v)
  def isNegated = false
}
case class NegativeAtom(v: Variable) extends Literal {
  def negated = PositiveAtom(v)
  def isNegated = true
}

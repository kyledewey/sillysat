package sillysat.syntax

// Everything is in CNF form
case class Variable(name: String)

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

// implicit disjunction between literals
case class Clause(literals: Seq[Literal]) {
  def variables: Set[Variable] =
    literals.map(_.v).toSet
}

// implicit conjunction between clauses
case class CNF(clauses: List[Clause]) {
  def addClause(c: Clause): CNF = CNF(c :: clauses)
  def variables: Set[Variable] =
    clauses.toSet.flatMap((c: Clause) => c.variables)
}


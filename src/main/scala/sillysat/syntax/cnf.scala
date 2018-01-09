package sillysat.syntax

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

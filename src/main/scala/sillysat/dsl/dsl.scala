package sillysat.dsl

import sillysat.syntax._

case class WrappedVariable(name: String, inverted: Boolean) {
  def not: WrappedVariable = WrappedVariable(name, !inverted)
  def asLiteral: Literal = {
    val v = Variable(name)
    if (!inverted) {
      PositiveAtom(v)
    } else {
      NegativeAtom(v)
    }
  }
}

case class WrappedDisjunction(lits: Seq[WrappedVariable]) {
  def or(other: WrappedDisjunction): WrappedDisjunction =
    WrappedDisjunction(lits ++ other.lits)
  def asClause(): Clause =
    Clause(lits.map(_.asLiteral))
}

case class WrappedConjunction(clauses: Seq[WrappedDisjunction]) {
  def and(other: WrappedConjunction): WrappedConjunction =
    WrappedConjunction(clauses ++ other.clauses)
  def asCNF(): CNF =
    CNF(clauses.map(_.asClause).toList)
}

object DSL {
  import scala.language.implicitConversions

  implicit def symbolToVariable(sym: Symbol): WrappedVariable =
    WrappedVariable(sym.name, false)

  implicit def symbolToDisjunction(sym: Symbol): WrappedDisjunction =
    WrappedDisjunction(Seq(symbolToVariable(sym)))

  implicit def variableToDisjunction(v: WrappedVariable): WrappedDisjunction =
    WrappedDisjunction(Seq(v))

  implicit def disjunctionToConjunction(d: WrappedDisjunction): WrappedConjunction =
    WrappedConjunction(Seq(d))
}

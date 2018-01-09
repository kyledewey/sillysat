package sillysat.dsl

import sillysat.syntax._

object WrappedFormula {
  def disjunctionOfLiterals(f: WrappedFormula): Option[Seq[Literal]] = {
    f match {
      case WrappedNot(WrappedVariable(name)) =>
        Some(Seq(NegativeAtom(Variable(name))))
      case WrappedVariable(name) =>
        Some(Seq(PositiveAtom(Variable(name))))
      case WrappedOr(f1, f2) => {
        for {
          lits1 <- disjunctionOfLiterals(f1)
          lits2 <- disjunctionOfLiterals(f2)
        } yield lits1 ++ lits2
      }
      case _ => None
    }
  }

  // must be a disjunction of literals
  def asClause(f: WrappedFormula): Option[Clause] = {
    disjunctionOfLiterals(f).map(Clause.apply)
  }

  // must be a conjunction of clauses (see asClause)
  def asCNF(f: WrappedFormula): Option[CNF] = {
    asClause(f).map(c => CNF(List(c))) match {
      case res@Some(_) => res
      case None => {
        f match {
          case WrappedAnd(f1, f2) => {
            for {
              CNF(first) <- asCNF(f1)
              CNF(rest) <- asCNF(f2)
            } yield CNF(first ++ rest)
          }
          case _ => None
        }
      }
    }
  }
}

sealed trait WrappedFormula {
  def not: WrappedFormula = WrappedNot(this)
  def and(other: WrappedFormula): WrappedFormula = WrappedAnd(this, other)
  def or(other: WrappedFormula): WrappedFormula = WrappedOr(this, other)
  def asCNF: Option[CNF] = WrappedFormula.asCNF(this)
  def toFormula: Formula
}

case class WrappedVariable(name: String) extends WrappedFormula {
  def toFormula: Formula = Variable(name)
}
case class WrappedNot(f: WrappedFormula) extends WrappedFormula {
  def toFormula: Formula = Not(f.toFormula)
}
case class WrappedAnd(f1: WrappedFormula, f2: WrappedFormula) extends WrappedFormula {
  def toFormula: Formula = And(f1.toFormula, f2.toFormula)
}
case class WrappedOr(f1: WrappedFormula, f2: WrappedFormula) extends WrappedFormula {
  def toFormula: Formula = Or(f1.toFormula, f2.toFormula)
}

object DSL {
  import scala.language.implicitConversions

  implicit def symbolToVariable(sym: Symbol): WrappedVariable = {
    WrappedVariable(sym.name)
  }
}

package sillysat.solver

import scala.annotation.tailrec
import sillysat.syntax._

object Solver {
  // determines if a given clause is satisfied based on assignments so far
  // if it's satisfied, then adding any assignments will not change this
  // however, if it's unsatisfied, then it is possible that it will
  // later become satisfied due to variable assignments added later
  def isSatisfied(clause: Clause, assignments: Map[Variable, Boolean]): Boolean =
    clause.literals.exists(lit =>
      assignments.get(lit.v).map(binding =>
        if (lit.isNegated) !binding else binding).getOrElse(false))

  def unassignedLiterals(clause: Clause, assignments: Map[Variable, Boolean]): Set[Literal] = 
    clause.literals.filter(lit => !assignments.contains(lit.v)).toSet

  // gets the variable that needs to be assigned in a unit clause,
  // or None if the given clause isn't a unit clause
  def unitLiteral(clause: Clause, assignments: Map[Variable, Boolean]): Option[Literal] = {
    // a unit clause has one unassigned variable, and the clause
    // is yet unsatisfied
    if (!isSatisfied(clause, assignments)) {
      val unassigned = unassignedLiterals(clause, assignments)
      if (unassigned.size == 1) {
        Some(unassigned.head)
      } else {
        None
      }
    } else None
  }

  // If the given clause is a unit clause, then it will
  // make an assignment based on it.
  def tryApplyUnit(clause: Clause, assignments: Map[Variable, Boolean]): Map[Variable, Boolean] = {
    unitLiteral(clause, assignments) match {
      case Some(lit) => {
        assert(!assignments.contains(lit.v))
        assignments + (lit.v -> !lit.isNegated)
      }
      case None => {
        assignments
      }
    }
  }

  def alwaysUnsatisfied(clause: Clause, assignments: Map[Variable, Boolean]): Boolean =
    clause.literals.forall(lit =>
      assignments.contains(lit.v) && assignments(lit.v) == lit.isNegated)

  // attempts to apply the unit clause rule to all clauses
  // returns either a new series of assignments or None to indicate a conflict was found
  def applyUnitToAll(clauses: List[Clause], assignments: Map[Variable, Boolean]): Option[Map[Variable, Boolean]] = {
    // highly inefficient O(n^2) algorithm
    @tailrec
    def recur(cls: List[Clause], asn: Map[Variable, Boolean]): Option[Map[Variable, Boolean]] = {
      cls match {
        case head :: tail => {
          if (alwaysUnsatisfied(head, asn)) {
            None
          } else {
            val newAsn = tryApplyUnit(cls.head, asn)
            if (newAsn ne asn) {
              // assignment made - start again from beginning
              recur(clauses, newAsn)
            } else {
              // assignment not made - continue processing
              recur(tail, asn)
            }
          }
        }
        case Nil =>
          Some(asn)
      }
    }

    recur(clauses, assignments)
  }

  def solve(problem: CNF): Option[Map[Variable, Boolean]] = {
    val clauses = problem.clauses
    val vars = problem.variables
    def doSolve(asn: Map[Variable, Boolean]): Option[Map[Variable, Boolean]] = {
      applyUnitToAll(clauses, asn) match {
        case Some(newAsn) => {
          val diff = vars -- newAsn.keys
          if (diff.isEmpty) {
            // we have an assignment for all variables
            Some(newAsn)
          } else {
            // some variables are unspoken for - choose one and set a value
            val choice = diff.head
            doSolve(asn + (choice -> true)) match {
              case s@Some(_) => s
              case None => doSolve(asn + (choice -> false))
            }
          }
        }
        case None =>
          None
      }
    }
    
    doSolve(Map()) match {
      case s@Some(soln) => {
        assert(soln.keySet == vars)
        assert(clauses.forall(c => isSatisfied(c, soln)))
        s
      }
      case None => None
    }
  }
}

object Test {
  import sillysat.dsl._
  import sillysat.dsl.DSL._

  // (x || -y) && (-y || z)
  val test1 = ('x or 'y.not) and ('y.not or 'z)

  val test2 = ('x1.not or 'x2) and 
              ('x1.not or 'x3 or 'x5) and
              ('x2.not or 'x4) and
              ('x3.not or 'x4.not) and
              ('x1 or 'x5 or 'x2.not) and
              ('x2 or 'x3) and
              ('x2 or 'x3.not) and
              ('x6 or 'x5.not)
}

package sillysat.solver

import scala.annotation.tailrec
import sillysat.syntax._

case class SolverState(assignments: Map[Variable, Boolean], implGraph: Map[Variable, (Clause, Variable)]) {
  assert(implGraph.keys.forall(k => assignments.contains(k)))

  def makeChoice(variable: Variable, choice: Boolean): SolverState = {
    assert(!assignments.contains(variable))
    copy(assignments = assignments + (variable -> choice))
  }

  def addAssignment(becauseOf: Option[Variable], clause: Clause, changing: Literal): SolverState = {
    assert(!assignments.contains(changing.v))
    val newGraph = 
      if (becauseOf.isEmpty || implGraph.contains(becauseOf.get)) {
        implGraph
      } else {
        implGraph + (becauseOf.get -> (clause, changing.v))
      }
    
    copy(assignments = assignments + (changing.v -> !changing.isNegated),
         implGraph = newGraph)
  }

  def roots(): Set[Variable] =
    implGraph.keySet -- implGraph.values.map(_._2)

  // if there aren't any roots, then the clause must be unsat - this
  // indicates that we made no arbitrary decisions
  def makeConflictClause(): Option[Clause] = {
    val rts = roots
    if (rts.isEmpty) {
      None
    } else {
      Some(
        Clause(
          rts.map(v =>
            if (assignments(v)) NegativeAtom(v) else PositiveAtom(v)).toSeq))
    }
  }
}

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
  // make an assignment based on it.  Takes the variable which changed
  // which forced the unit clause switch, and returns a tuple of the new
  // solver state along with which variable was changed.  If it changes nothing,
  // it returns None
  def tryApplyUnit(clause: Clause, becauseOf: Option[Variable], state: SolverState): Option[(SolverState, Variable)] =
    unitLiteral(clause, state.assignments).map(lit => 
      (state.addAssignment(becauseOf, clause, lit), lit.v))

  def alwaysUnsatisfied(clause: Clause, assignments: Map[Variable, Boolean]): Boolean =
    clause.literals.forall(lit =>
      assignments.contains(lit.v) && assignments(lit.v) == lit.isNegated)
  
  // this means that both a positive atom and a negative atom with the
  // same underlying variable is found
  def literalsConflict(lits: List[Literal]): Boolean =
    lits.foldLeft(Map[Variable, Set[Literal]]())((res, cur) =>
      res + (cur.v -> (res.get(cur.v).getOrElse(Set()) + cur))).values.exists(_.size == 2)
  
  // attempts to apply the unit clause rule to all clauses
  // returns either a new solver state or a clause which explains a conflict
  // repeats this process until a fixpoint is reached
  def applyUnitToAll(clauses: List[Clause], becauseOf: Option[Variable], state: SolverState): Either[Option[Clause], SolverState] = {
    @tailrec
    def recur(cls: List[Clause], becauseOf: Option[Variable], state: SolverState): Either[Option[Clause], SolverState] = {
      cls match {
        case head :: tail => {
          if (alwaysUnsatisfied(head, state.assignments)) {
            Left(state.makeConflictClause)
          } else {
            tryApplyUnit(cls.head, becauseOf, state) match {
              case Some((newState, variableChanged)) =>
                // assignment made - start again from the beginning
                // TODO: This doesn't make the implication graph correctly,
                // as it will skip over any other unit clauses that could
                // still be applied without this assignment
                recur(clauses, Some(variableChanged), newState)
              case None =>
                // assignment not made - continue processing
                recur(tail, becauseOf, state)
            }
          }
        }
        case Nil =>
          Right(state)
      }
    }

    recur(clauses, becauseOf, state)
  }

  def solve(problem: CNF): Option[Map[Variable, Boolean]] = {
    var isUnsat = false
    var clauses = problem.clauses
    val vars = problem.variables
    def doSolve(becauseOf: Option[Variable], state: SolverState): Option[SolverState] = {
      if (isUnsat) return None

      applyUnitToAll(clauses, becauseOf, state) match {
        case Right(newState) => {
          val diff = vars -- newState.assignments.keys
          if (diff.isEmpty) {
            // we have an assignment for all variables
            Some(newState)
          } else {
            // some variables are unspoken for - choose one and set a value
            val choice = diff.head
            doSolve(Some(choice), newState.makeChoice(choice, true)) match {
              case s@Some(_) => s
              case None => doSolve(Some(choice), newState.makeChoice(choice, false))
            }
          }
        }
        case Left(Some(c)) => {
          clauses ::= c
          println("ADDING IMPLIED CLAUSE: " + c)
          None
        }
        case Left(None) => {
          isUnsat = true
          None
        }
      }
    }
    
    doSolve(None, SolverState(Map(), Map())).map(soln => {
      assert(soln.assignments.keySet == vars)
      assert(clauses.forall(c => isSatisfied(c, soln.assignments)))
      soln.assignments
    })
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

  val test3 = symbolToDisjunction('x) and WrappedDisjunction(Seq('x.not))

  val test4 = ('a or 'b or 'c) and ('b or 'c.not or 'f.not) and ('b.not or 'e)

  val test5 = ('a or 'b) and ('a or 'b.not) and ('a.not or 'c) and ('a.not or 'c.not)
}

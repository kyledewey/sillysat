package sillysat.solver

import scala.annotation.tailrec
import sillysat.syntax._

case class SolverState(assignments: Map[Variable, Boolean], implGraph: Map[Variable, (Clause, Variable)], atLevels: Map[Variable, Int]) {
  assert(implGraph.keys.forall(k => assignments.contains(k)))

  def makeChoice(variable: Variable, choice: Boolean, level: Int): SolverState = {
    assert(!assignments.contains(variable))
    assert(!atLevels.contains(variable))
    copy(assignments = assignments + (variable -> choice),
         atLevels = atLevels + (variable -> level))
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

  // If there aren't any roots, then the clause must be unsat - this
  // indicates that we made no arbitrary decisions.  We return both
  // a clause that's learned along with what level we can safely backtrack to.
  def makeConflictClause(): Option[(Clause, Int)] = {
    assert(roots.forall(atLevels.contains))

    val rts = roots
    if (rts.isEmpty) {
      None
    } else {
      Some(
        (Clause(
          rts.map(v =>
            if (assignments(v)) NegativeAtom(v) else PositiveAtom(v)).toSeq),
         rts.map(root => atLevels(root)).max))
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
  def applyUnitToAll(clauses: List[Clause], becauseOf: Option[Variable], state: SolverState): Either[Option[(Clause, Int)], SolverState] = {
    @tailrec
    def recur(cls: List[Clause], becauseOf: Option[Variable], state: SolverState): Either[Option[(Clause, Int)], SolverState] = {
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
    import scala.collection.mutable.Stack
    var clauses = problem.clauses
    val vars = problem.variables
    // decisions to try in case of backtracking
    var tryFalseStack = Stack[(Variable, SolverState)]() 
    var shouldRun = true
    var justFlipped: Option[Variable] = None
    var currentState = SolverState(Map(), Map(), Map())
    var retval: Option[Map[Variable, Boolean]] = None

    while (shouldRun) {
      // apply binary constraint propagation
      applyUnitToAll(clauses, justFlipped, currentState) match {
        case Right(newState) => {
          // We have no conflicts at this point.  See which variables still need
          // assignment.
          val diff = vars -- newState.assignments.keys
          if (diff.isEmpty) {
            // we have an assignment for all variables
            // ensure everything is satisfied
            assert(clauses.forall(c => isSatisfied(c, newState.assignments)))
            retval = Some(newState.assignments)
            shouldRun = false
          } else {
            // some variables still need assignments - choose one and set a value
            val choice = diff.head

            // in case of failure, pop until the stack is of this length
            val decisionLevel = tryFalseStack.size
            // note that we still need to try the false path
            tryFalseStack.push((choice, 
                                newState.makeChoice(choice, false, decisionLevel)))

            // try with true
            currentState = newState.makeChoice(choice, true, decisionLevel)
            justFlipped = Some(choice)
          }
        }
        case Left(Some((c, dl))) => {
          // We hit a conflict, and we have a learned clause that illustrates what needs
          // to be asserted in order to ensure we don't hit this same conflict again
          clauses ::= c
          
          // backtrack up to the level specified
          println("STACK SIZE: " + tryFalseStack.size)
          println("DL: " + dl)

          val stackSize = tryFalseStack.size
          assert(dl <= stackSize)

          val popUpTimes = tryFalseStack.size - dl - 1
          (0 until popUpTimes).foreach(_ => tryFalseStack.pop)
          
          // try from the last backtracking point
          if (tryFalseStack.nonEmpty) {
            val (v, state) = tryFalseStack.pop
            currentState = state
            justFlipped = Some(v)
          } else {
            shouldRun = false
          }
        }
        case Left(None) => {
          // We hit a conflict, not due to any particular choice.  This can only
          // happen when the toplevel clause is unsatisfiable
          shouldRun = false
        }
      } // applyUnitToAll match
    } // while (shouldRun)

    retval
  } // solve
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

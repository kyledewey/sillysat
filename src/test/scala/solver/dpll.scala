package sillysat.solver

import sillysat.dsl._

class TestDPLL extends TestSolver {
  def trySolve(f: WrappedFormula): Option[TestResult] = {
    f.asCNF.map(cnf =>
      TestSolver.optionToTestResult(DPLL.solve(cnf)))
  }
}


package sillysat.solver

import sillysat.dsl._

class TestTableaux extends TestSolver {
  def trySolve(f: WrappedFormula): Option[TestResult] = {
    Some(TestSolver.optionToTestResult(Tableaux.solve(f.toFormula)))
  }
}

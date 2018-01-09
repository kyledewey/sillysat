package sillysat.solver

import sillysat.dsl._

import org.scalatest.FlatSpec

abstract class TestSolver extends FlatSpec {
  def trySolve(f: WrappedFormula): Option[TestResult]

  private def runTest(f: WrappedFormula, exp: TestResult) {
    assertResult(Some(exp)) {
      trySolve(f)
    }
  }

  "The solver" should "handle trivial truth" in {
    runTest(DSL.symbolToVariable('a), Satisfiable)
  }

  Tests.allTests.foreach( { case (f, exp) => {
    it should ("handle " + f.toFormula.toString) in {
      runTest(f, exp)
    }
  } } )
}

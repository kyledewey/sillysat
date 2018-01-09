package sillysat.solver

import sillysat.dsl._

import org.scalatest.FlatSpec

object TestSolver {
  def optionToTestResult[A](op: Option[A]): TestResult = {
    op match {
      case Some(_) => Satisfiable
      case None => Unsatisfiable
    }
  }
}

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

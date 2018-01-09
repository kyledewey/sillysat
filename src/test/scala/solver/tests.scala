package sillysat.solver

sealed trait TestResult
case object Satisfiable extends TestResult
case object Unsatisfiable extends TestResult

object Tests {
  import sillysat.dsl._
  import sillysat.dsl.DSL._

  // (x || -y) && (-y || z)
  val test1 = ('x or 'y.not) and
              ('y.not or 'z)

  val test2 = ('x1.not or 'x2) and 
              ('x1.not or 'x3 or 'x5) and
              ('x2.not or 'x4) and
              ('x3.not or 'x4.not) and
              ('x1 or 'x5 or 'x2.not) and
              ('x2 or 'x3) and
              ('x2 or 'x3.not) and
              ('x6 or 'x5.not)

  val test3 = 'x and 'x.not

  val test4 = ('a or 'b or 'c) and
              ('b or 'c.not or 'f.not) and
              ('b.not or 'e)

  val test5 = ('a or 'b) and
              ('a or 'b.not) and
              ('a.not or 'c) and
              ('a.not or 'c.not)

  val allTests: Map[WrappedFormula, TestResult] =
    scala.collection.immutable.ListMap(test1 -> Satisfiable,
                                       test2 -> Satisfiable,
                                       test3 -> Unsatisfiable,
                                       test4 -> Satisfiable,
                                       test5 -> Unsatisfiable)
}

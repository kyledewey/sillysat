package sillysat.dsl

import sillysat.syntax._
import sillysat.dsl.DSL._

import org.scalatest.FlatSpec

class TestDSL extends FlatSpec {
  "The DSL" should "handle conjunction of positive literals" in {
    assertResult(Some(CNF(List(Clause(Seq(PositiveAtom(Variable("a")))),
                               Clause(Seq(PositiveAtom(Variable("b")))))))) {
      ('a).and('b).asCNF
    }
  }

  it should "handle conjunction of negative literals" in {
    assertResult(Some(CNF(List(Clause(Seq(NegativeAtom(Variable("a")))),
                               Clause(Seq(NegativeAtom(Variable("b")))))))) {
      ('a).not.and(('b).not).asCNF
    }
  }

  it should "handle conjunction of mixed literals - positive first" in {
    assertResult(Some(CNF(List(Clause(Seq(PositiveAtom(Variable("a")))),
                               Clause(Seq(NegativeAtom(Variable("b")))))))) {
      ('a).and(('b).not).asCNF
    }
  }

  it should "handle conjunction of mixed literals - positive second" in {
    assertResult(Some(CNF(List(Clause(Seq(NegativeAtom(Variable("a")))),
                               Clause(Seq(PositiveAtom(Variable("b")))))))) {
      ('a).not.and('b).asCNF
    }
  }
}

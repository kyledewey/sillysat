package sillysat.solver

import sillysat.syntax._

object LiteralSet {
  def apply(x: Variable, b: Boolean): Option[LiteralSet] = {
    Some(LiteralSet(Map(x -> b)))
  }

  def apply(): LiteralSet = {
    LiteralSet(Map())
  }
}

case class LiteralSet(holds: Map[Variable, Boolean]) {
  def add(x: Variable, b: Boolean): Option[LiteralSet] = {
    holds.get(x) match {
      case Some(`b`) => Some(this) // already has mapping
      case Some(_) => None // different mapping
      case None => Some(LiteralSet(holds + (x -> b))) // new variable
    }
  }

  def merge(other: LiteralSet): Option[LiteralSet] = {
    other.holds.foldLeft(Some(this): Option[LiteralSet])((res, cur) =>
      res.flatMap(_.add(cur._1, cur._2)))
  }
}

object Tableaux {
  type GoalStack = List[(Formula, Boolean)]
  case class Choice(f: Formula, negated: Boolean, andThen: GoalStack, under: LiteralSet)

  def solve(f: Formula): Option[LiteralSet] = {
    solve(List((f -> false)), LiteralSet(), List())
  }

  @scala.annotation.tailrec
  private def solve(gs: GoalStack, under: LiteralSet, choices: List[Choice]): Option[LiteralSet] = {
    gs match {
      case (formula, negated) :: goals => {
        formula match {
          case x: Variable => {
            under.add(x, !negated) match {
              case Some(under) => {
                solve(goals, under, choices)
              }
              case None => {
                // restart at another choice
                choices match {
                  case Choice(f, negated, andThen, under) :: rest => {
                    solve((f -> negated) :: andThen, under, rest)
                  }
                  case _ => None
                }
              }
            }
          }
          case Not(formula) => {
            solve((formula -> !negated) :: goals, under, choices)
          }
          case And(f1, f2) if !negated => {
            solve((f1 -> false) :: (f2 -> false) :: goals, under, choices)
          }
          case And(f1, f2) if negated => {
            // De Morgans: !(A && B) == !A || B
            solve((Or(Not(f1), Not(f2)) -> false) :: goals, under, choices)
          }
          case Or(f1, f2) if !negated => {
            solve((f1 -> false) :: goals,
                  under,
                  Choice(f2, false, goals, under) :: choices)
          }
          case Or(f1, f2) if negated => {
            // De Morgans: !(A || B) == !A && !B
            solve((And(Not(f1), Not(f2)) -> false) :: goals, under, choices)
          }
        }
      }
      // empty goal stack - trivially true
      case _ => Some(under)
    }
  }
}

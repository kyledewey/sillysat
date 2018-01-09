package sillysat.syntax

sealed trait Formula
case class Variable(name: String) extends Formula
case class Not(f: Formula) extends Formula
case class And(f1: Formula, f2: Formula) extends Formula
case class Or(f1: Formula, f2: Formula) extends Formula

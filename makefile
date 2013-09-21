all: src/syntax.scala src/solver.scala src/dsl.scala
	fsc-2.10 -d sillysat src/syntax.scala src/solver.scala src/dsl.scala

clean:
	rm -rf sillysat/*

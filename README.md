# evolution-aware-analysis

This repository contains the code for the implementation and
evaluation of the evolution-aware analysis method described in my
undergraduate thesis.

All the programs are written in Racket. The main files of interest are:
- `meta.rkt` :: contains the implementation of the metainterpreter
  that enables us to obtain an evolution-aware analysis from a plain
  analysis.
- `tree-analysis.rkt` :: code for the `tree-count-even`
  evaluation scenario.
- `expr-depth.rkt` :: code for the `expr-depth` scenario

These last two can be run with the Racket program in order to execute
the evaluation scenarios described in the paper.  First a correctness
test is executed, then a timed execution test is ran. After
running, the generated data will be placed in the `experimental-data`
directory, and the graphs will be placed in the `plots` directory.

The 100KLOC test file used for the `expr-depth` is `test.rkt` in this
repository. Furthermore, the file `lisp-analysis.rkt` contains utility
functions for dealing with s-expression evolution, including the
functions that are used to randomly mutate an expression.

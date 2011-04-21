CL-PATTERN
==========

CL-PATTERN is a very fast ML-like pattern-matching library for Common
Lisp.

API
---

### Macro: `match`

    match value &body clauses

`match` macro tries to match `value` with `clauses` and raise an error
if no clauses matched. A clause have to be a form of `(pattern
form*)`, where `pattern` is a pattern (see "Patterns").

#### Examples

    (match '(1 2)
      ((x y) (+ x y)))
    ;;=> 3

    (match '(x 1)
      (('x x) x))
    ;;=> 1

    (match '(1 2)
      ((1 &optional a) a))
    ;;=> 2

### Macro: `lambda-match`

    lambda-match &body clauses

`lambda-match` is a shorthand of `lambda` and `match`. For example,

    (lambda-match
      (('foo a) a))

will be expaneded to

    (lambda (arg)
      (match arg
        (('foo a) a)))

### Patterns

A pattern must be one of a symbol, a cons, and an atom. If the pattern
is symbol, the pattern is called a variable pattern, which can be
matched with any value. A body of a clause will be evaluated with
using a binding of the variable and the valueThe variable can be used
in a body of a. If the variable is `_`, any binding will not be
made. Here is an example:

    (match 1
      (x x))
    ;;=> 1

If the pattern is a cons, there is three cases you have to know. If a
`car` of the cons is `quote`, the pattern is called a constant
pattern, which can be matched with a same value of a `cadr` of the
cons. Here is an example:

    (match 'x
      ('x 1))
    ;;=> 1

Second case, if the `car` of the cons is `&optional`, the pattern is
called a optional variable pattern, which can be matched with any
value as same as usual variable patterns, but it can be not
matched. If not matched, we say the pattern is unbound, meaning an
undefined value (`nil` will bound to the pattern.

    (match '(1)
      ((1 &optional x) x))
    ;;=> NIL
    
    (match '(1 2)
      ((1 &optional x) x))
    ;;=> 2

Othewise, the pattern is called a structure pattern, which can be
matched if a `car` of the pattern and a `cdr` of the pattern are
matched with a value. Here is an example:

    (match '(1 . 2)
      ((x . y) (+ x y)))
    ;;=> 1

As you may guess, the `car` of the pattern and the `cdr` of the
pattern are also patterns. So you can nest patterns infinitely.

If the pattern is an atom, the pattern is called a constant pattern
too. As we said, the pattern can be matched with a same value of the
pattern. Here is an example:

    (match 1
      (1 'matched))
    ;;=> MATCHED

Micro Benchmark
---------------

Here is a micro benchmark comparing

    (match triple
      ((a &optional b c) (+ a b c)))

and

    (destructuring-bind (a &optional b c)
        triple
      (+ a b c))

* 10000000 times
* with `(declare (optimized (speed 3)))`
* on Core 2 Duo 1.6GHz

### Allegro CL v8.2 (Express Edition)

On Allegro CL, `destructuring-bind` seems to be tuned properly.

    MATCH
    ; cpu time (non-gc) 0.190000 sec user, 0.000000 sec system
    ; cpu time (gc)     0.000000 sec user, 0.000000 sec system
    ; cpu time (total)  0.190000 sec user, 0.000000 sec system
    ; real time  0.188305 sec
    ; space allocation:
    ;  0 cons cells, 0 other bytes, 0 static bytes
    DESTRUCTURING-BIND
    ; cpu time (non-gc) 0.040000 sec user, 0.000000 sec system
    ; cpu time (gc)     0.000000 sec user, 0.000000 sec system
    ; cpu time (total)  0.040000 sec user, 0.000000 sec system
    ; real time  0.040888 sec
    ; space allocation:
    ;  0 cons cells, 0 other bytes, 0 static bytes

### SBCL v1.0.47

On SBCL, even in such the simple case, `destructuring-bind` seems to
be a very high cost operation.

    MATCH
    Evaluation took:
      0.007 seconds of real time
      0.010000 seconds of total run time (0.010000 user, 0.000000 system)
      142.86% CPU
      10,040,848 processor cycles
      0 bytes consed
      
    DESTRUCTURING-BIND
    Evaluation took:
      0.983 seconds of real time
      0.980000 seconds of total run time (0.970000 user, 0.010000 system)
      99.69% CPU
      1,568,842,248 processor cycles
      0 bytes consed

### ECL v11.1.1

On ECL, `match` and `destructuring-bind` are almost same cost in this
simple case.

    MATCH
    real time : 0.883 secs
    run time  : 0.890 secs
    gc count  : 2 times
    consed    : 66339433 bytes
    DESTRUCTURING-BIND
    real time : 0.970 secs
    run time  : 0.960 secs
    gc count  : 1 times
    consed    : 66346216 bytes

Supported Implementations
-------------------------

* Allegro CL v8.2
* SBCL v1.0.47
* CMU CL v20b
* Clozure CL v1.6
* ECL v11.1.1
* GNU CLISP v2.48

----

Copyright (C) 2011  Tomohiro Matsuyama <<tomo@cx4a.org>>.

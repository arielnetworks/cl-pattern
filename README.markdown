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

`match` macro establishes new match parameters of that `test` function
is `equal` and `unbound` is nil. See "Macro Parameters" for details.

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

### Macro: `match*`

    match* value &body clauses

Same as `match` macro except that `match*` accepts the current
(lexical) match parameters. That is, you must be careful of the
current lexical context if you use this macro. See "Match Parameters"
for details.

#### Examples

    (with-match-parameters (:unbound t)
      (match* ()
        ((&optional v) v)))
    ;;=> T
    
    (with-match-parameters (:test string-equal)
      (match* :foo
        ("foo" 'matched)))
    ;;=> MATCHED

### Macro: `with-match-parameters`

    with-match-parameters (&key test unbound) &body body

Establishes match parameters for lexical scope of `body`. This only
affects on `match*` macro because `match` establishes another match
parameters by itself.

`test` keyword parameter will be used for comparing a constant patten
(e.g. 1, "a", :x, etc) with a matching variable. For example,

    (with-match-parameters (test string-equal)
      ...)

makes compare expressions within its body to use `string-equal` for
comparing patterns and values. A value of this keyword parameter must
be a function name or a lambda expression to achieve better
performance. The default value is `equal`.

`unbound` keyword parameter will be used for specifying a value of
unbound variables. Optional variables in a pattern might be unbound
even if the pattern is matched. In that case, such optional variables
have a value specified by `unbound` keyword parameter. The default
value is `nil`.

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
matched with any value. The variable can be used in a body of a
clause. Here is an example:

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
matched. If not matched, we say the pattern is unbound, meaning some
undefined value will bound to the pattern. See "Match Parameters" for
details. Here is an example:

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

### Match Parameters

Match parameters controls a meaning of pattern-matching in
lexical. This will be used when you want to

* change a default equal function for comparing patterns and values.
* change a default value of optional variables.

Note that `match` macro establishes a new match parameters by itself,
meaning that doesn't accept the current lexical match parameters. In
contrast, `match*` accept the current lexical match parameters. This
is the main difference between `match` and `match*`.

See `with-match-parameters` for details.

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

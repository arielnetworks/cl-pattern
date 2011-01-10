cl-pattern
=======

cl-pattern is a Common Lisp library that provides ML-like (OCaml, Haskell, etc) pattern matching facilities.

Usage
-----

### match

    (match value &body clauses)

`match` macro tries to match `value` with `clauses` and raise errors if no clauses matched. Clause have to be a form of `(pattern* form*)`, where `pattern` can be a symbol for binding values, a atom for matching values, and a composed form of them (with `destructuring-bind`). Binding to `_` will be ignored (i.e. `(declare (ignore _))`) automatically and its binding can't be used anywhere. If `pattern` is one of `t`, `otherwise`, and `_`, its clause will be matched with any value.

#### Examples

    (match 1
      (x x))
    ; => 1
    
    (match '(1 2)
      ((x y) (+ x y)))
    ; => 3
    
    (match '(:bar 1)
      ((:foo _) "Foo!")
      ((:bar _) "Bar!"))
    ; => "Bar!"
    
    (match '(x 1)
      (('x x) x))
    ; => 1
    
    (match 5
      (1 'one)
      (2 'two)
      (3 'three)
      (4 'four))
    ; => match error
    
    (match 5
      (1 'one)
      (2 'two)
      (3 'three)
      (4 'four)
      (t 'otherwise))
    ; => OTHERWISE
    
    (match '("a" :x 1)
      ((a &key x)
       (list a x)))
    ; => ("a" 1)
    
    (defun sum (lst)
      (match lst
        (() 0)
        ((x . xs) (+ x (sum xs)))))
    (sum '(1 2 3))
    ; => 6

### let+

    (let+ bindings &body body)

Unlike `let` macro, `let+` macro tries to bind variables with pattern using `match` macro. Binding have to be a form of `(pattern+ form)`, where `pattern` is of `match` macro. If multiple patterns are given, `let+` macro assumes that `form` returns multiple values and bind them with `multiple-value-bind`. Binding variables is not simultaneous but sequential like `let*` macro.

#### Examples

    (let+ ((x 1))
      x)
    ; => 1
    
    (let+ (((x y) '(1 2)))
      (+ x y))
    ; => 3
    
    (let+ (((x y) '(1)))
      (+ x y))
    ; => match error
    
    (let+ ((a b c (values 1 2 3)))
      (list a b c))
    ; => (1 2 3)
    
    (let+ ((a 1)
           (b a))
      b)
    ; => 1

License
-------

Copyright (C) 2011  Tomohiro Matsuyama <<tomo@cx4a.org>>.
Licensed under the LLGPL License.

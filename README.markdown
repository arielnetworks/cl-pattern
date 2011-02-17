cl-pattern
==========

cl-pattern is a very fast ML-like pattern-matching library for Common Lisp.

Usage
-----

### match

    (match value &body clauses)

`match` macro tries to match `value` with `clauses` and raise errors if no clauses matched. Clause have to be a form of `(pattern form*)`, where `pattern` is a symbol for binding values, an atom for matching values, and a composed form of them. Binding to `_` will be ignored (i.e. `(declare (ignore _))`) automatically and its binding can't be used anywhere. If `pattern` is `_`, its clause will be matched with any value.

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
    
    (defun sum (lst)
      (match lst
        (() 0)
        ((x . xs) (+ x (sum xs)))))
    (sum '(1 2 3))
    ; => 6

License
-------

Copyright (C) 2011  Tomohiro Matsuyama <<tomo@cx4a.org>>.
Licensed under the LLGPL License.

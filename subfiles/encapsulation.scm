; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

;;;;;;;;;;;;;;;;;;
; encapsulations ;
;;;;;;;;;;;;;;;;;;
;
; An encapsulation has type 'encapsulation, and attributes 'counter and 'value.
; When viewed from within Kernel, the counter is part of the type.
;
; Each call to procedure make-encapsualtion-type returns a matched set of
; encapsulator/type-predicate/decapsulator using a unique counter;
; the encapsulator manufactures encapsulations with that counter,
; the type-predicate returns true only for encapsulations with that counter,
; and the decapsulator only works on encapsulations with that counter.
;

(library (subfiles encapsulation)
  (export make-encapsulation-type)
  (import (rnrs)
          (subfiles revision)
          (subfiles object)
          (subfiles kernel-pair)
          (subfiles applicative))

(define make-encapsulation-type
  (let ((counter  0))
    (lambda ()
      (set! counter (+ counter 1))
      (let ((counter  counter))
        (let ((this-type?  (lambda (x)
                             (and (object? x)
                                  (eq? (x 'type) 'encapsulation)
                                  (= (x 'counter) counter)))))
          (kernel-list
            (naive->checked-applicative
              (lambda (operand-tree)
                (let ((value  (kernel-car operand-tree))
                      (name   (list #t)))
                  (lambda (message)
                    (case message
                      ((type)    'encapsulation)
                      ((name)    name)
                      ((counter) counter)
                      ((value)   value)))))
              "encapsulator"
              1 1)
            (unary-predicate->applicative this-type?)
            (naive->checked-applicative
              (lambda (operand-tree)
                ((kernel-car operand-tree) 'value))
              "decapsulator"
              1 1 this-type?)))))))

(set-version (list 0.0 0)
             (list 0.1 0))
(set-revision-date 2007 8 4)

)

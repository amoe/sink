; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

(set-version (list 0.0 0)
             (list 0.1 0))
(set-revision-date 2007 8 4)

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

;
; Creates bindings for this type in a given environment.
;
; This code should not use any internal knowledge of the encapsulation type.
; It appears in this file, rather than in "subfiles/ground.scm", simply
; because it is logically associated with the encapsulation type.
;
(define bind-encapsulation-primitives!
  (lambda (env)
    (add-bindings! env

      'make-encapsulation-type
      (action->checked-applicative
        (lambda x (make-encapsulation-type))
        0 0))))

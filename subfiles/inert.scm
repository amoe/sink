; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

(set-version (list 0.0 0)
             (list 0.1 0))
(set-revision-date 2007 8 4)

;;;;;;;;;
; inert ;
;;;;;;;;;
;
; The inert value has type 'inert and no attributes.
;

(define inert (let ((name  (list #f)))
                (lambda (message)
                  (case message
                    ((type) 'inert)
                    ((name) name)))))

(define inert? (make-object-type-predicate 'inert))

;
; Creates bindings for this type in a given environment.
;
; This code should not use any internal knowledge of the inert type (not
; that there's anything much to use).  It appears in this file, rather than
; in "subfiles/ground.scm", simply because it is logically associated with
; the inert type.
;
(define bind-inert-primitives!
  (lambda (env)
    (add-bindings! env

      'inert?  (unary-predicate->applicative inert?))))

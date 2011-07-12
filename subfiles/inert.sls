#!r6rs
; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

;;;;;;;;;
; inert ;
;;;;;;;;;
;
; The inert value has type 'inert and no attributes.
;

(library (subfiles inert)
  (export inert
          inert?)
  (import (rnrs)
          (subfiles object)
          (subfiles revision))

(define inert (let ((name  (list #f)))
                (lambda (message)
                  (case message
                    ((type) 'inert)
                    ((name) name)))))

(define inert? (make-object-type-predicate 'inert))

(set-version (list 0.0 0)
             (list 0.1 0))
(set-revision-date 2007 8 4)

)

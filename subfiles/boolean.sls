#!r6rs
; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt


;;;;;;;;;;;;
; booleans ;
;;;;;;;;;;;;

(library (subfiles boolean)
  (export and?)
  (import (rnrs)
          (subfiles revision))

;
; Given zero or more boolean arguments, returns their conjunction (i.e.,
; returns #t unless at least one of them is false).
;
(define and?
  (lambda ls
    (or (null? ls)
        (and (car ls)
             (apply and? (cdr ls))))))

(set-version (list 0.0 0)
            (list 0.1 0))
(set-revision-date 2007 8 5)

)

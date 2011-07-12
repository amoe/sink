#!r6rs
#!r6rs

; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

;;;;;;;;;;;;;;;;;;;;;;;;;
; interpreter top level ;
;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Although this file contains the top-level interpreter, it isn't the top-level
; file; it is just one of the files loaded by "sink.scm".
;
; The interpreter is a read-eval-print loop run on a child of the ground
; environment.  The top-level context always returns to the point where the
; interpreter ordered its construction, causing the inner let to rebind its
; symbol "context" and re-run its body, (if (context? context) ...).
;

(library (subfiles interpreter)
  (export interpreter
          rep-loop
          report-error)
  (import (rnrs)
          (subfiles revision)
          (subfiles ground)
          (subfiles object)
          (subfiles proxy-1)
          (subfiles proxy-2)
          (subfiles context)
          (subfiles port)
          (subfiles eval))

(define interpreter
  (lambda ()
    (let ((env  (make-standard-environment)))
      (suggest-object-name env 'the-global-environment)
      (let ((context  (make-top-level-context report-error)))
        (if (context? context)
            (begin
              (initialize-context-bindings ground-environment context)
              (rep-loop env context))
            'SINK-terminated)))))

;
; The read-eval-print loop, parameterized by the global environment and the
; top-level context.
;
(define rep-loop
  (lambda (env context)
    (display ">> ")
    (let ((exp  (kernel-read (get-kernel-current-input-port context)
                             context)))
      (newline)
      (if (eof-object? exp)
          (terminal-pass '() context))
      (kernel-write (eval exp env context)
                    (get-kernel-current-output-port context)
                    context)
      (newline)
      (newline)
      (rep-loop env context))))


(set-version (list 0.0 0)
             (list 0.1 0))
(set-revision-date 2007 8 4)

)

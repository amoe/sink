; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

(library (subfiles proxy-2)
  (export error-pass
          abnormally-pass
          terminal-pass)
  (import (rnrs)
          (rnrs mutable-pairs))

  ;
; Given an error descriptor and the context in which the error occurred,
; abnormally passes the error descriptor to an appropriate error-handling
; context.
;
(define error-pass
  (lambda (descriptor source)
    (abnormally-pass descriptor source (source 'error-context))))

;
; Given a value and the context in which interpreter termination is requested,
; abnormally passes the value to that context's terminal-context.
;
(define terminal-pass
  (lambda (descriptor source)
    (abnormally-pass descriptor source (source 'terminal-context))))

;
; Abnormally passes a value from within a source context to a destination
; context.
;
(define abnormally-pass
  (letrec (;
           ; Given a context and a boolean, stores the boolean in the cars of
           ; the marks of all the ancestors of the context.
           ;
           (set-marks!
             (lambda (context boolean)
               (if (not (null? context))
                   (begin
                     (set-car! (context 'mark) boolean)
                     (set-marks! (context 'parent) boolean)))))
           ;
           ; Given a list of guards and a list of previously selected
           ; interceptors, and assuming that all ancestors of a target context
           ; are marked, selects at most one interceptor whose selector
           ; contains the target and prepends it to the list.  Returns the
           ; updated list of selected interceptors.
           ;
           (select-at-most-one
             (lambda (guards previously-selected)
               (cond ((null? guards)
                        previously-selected)
                     ((or (null? (caar guards))
                          (car ((caar guards) 'mark)))
                        (cons (cdar guards)
                              previously-selected))
                     (else (select-at-most-one (cdr guards)
                                               previously-selected)))))
           ;
           ; Given a context that contains the destination, and a list of
           ; selected entry-interceptors strictly below the given context, and
           ; assuming that all ancestors of the source are marked, returns a
           ; list of all selected entry-interceptors for the abnormal pass.
           ;
           (select-entry-interceptors
             (lambda (context previously-selected)
               (if (or (null? context)
                       (car (context 'mark)))
                   previously-selected
                   (select-entry-interceptors
                     (context 'parent)
                     (select-at-most-one
                       (context 'entry-guards)
                       previously-selected)))))
           ;
           ; Given a context that contains the source, and a list of all
           ; selected entry-interceptors for the abnormal pass, and assuming
           ; that all ancestors of the destination are marked, returns a list
           ; of selected interceptors including exit-interceptors at or above
           ; the given context.
           ;
           (select-exit-interceptors
             (lambda (context previously-selected)
               (if (or (null? context)
                       (car (context 'mark)))
                   previously-selected
                   (select-at-most-one
                     (context 'exit-guards)
                     (select-exit-interceptors
                       (context 'parent)
                       previously-selected)))))
           ;
           ; Given a list of interceptors and an abnormally passed value, uses
           ; the interceptors in series to transform the value; i.e., the value
           ; is passed as an argument to the first interceptor, the output of
           ; the first is passed as an argument to the second, etc.
           ;
           (serial-transform
             (lambda (interceptors value)
               (if (null? interceptors)
                   value
                   (serial-transform (cdr interceptors)
                                     ((car interceptors) value))))))

    (lambda (value source destination)
      (set-marks! source #t)
      (let ((selected  (select-entry-interceptors destination '())))
        (set-marks! source #f)
        (set-marks! destination #t)
        (let ((selected  (select-exit-interceptors source selected)))
          (set-marks! destination #f)
          ((destination 'receiver) (serial-transform selected value)))))))

)

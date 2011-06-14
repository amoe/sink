; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

(set-version (list 0.0 0)
             (list 0.1 0))
(set-revision-date 2007 8 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; evaluator central logic ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; Evaluate an expression in an environment, with given youngest enclosing
; context.  The normal result of evaluation is returned; contexts are only
; used for extraordinary purposes, i.e., Kernel abnormal passing of values
; or Kernel keyed dynamic bindings, so most theoretical Kernel contexts don't
; actually have to be constructed.
;
(define eval
  (lambda (exp env context)
    (cond ((kernel-pair? exp)  (combine (eval (kernel-car exp) env context)
                                        (kernel-cdr exp)
                                        env
                                        context))
          ((symbol? exp)  (lookup exp env context))
          (else exp))))

;
; Evaluate a combination in an environment,
; with given youngest enclosing context.
;
(define combine
  (lambda (combiner operand-tree env context)
    (cond ((operative? combiner)
             (operate combiner operand-tree env context))
          ((applicative? combiner)
             (combine (unwrap combiner)
                      (map-eval operand-tree env context combiner)
                      env
                      context))
          (else
             (error-pass (make-error-descriptor
                           (list "Tried to call a non-combiner: "
                                 (list combiner)))
                         context)))))

;
; Evaluate a list of expressions, and return a list of the results, with given
; youngest enclosing context; given also the applicative for which the list is
; being provided, just in case it's needed for an error message.
;
(define map-eval
  (lambda (operand-tree env context applicative)
    (if (not (kernel-list? operand-tree))
        (error-pass
          (make-error-descriptor
            (list "Operand tree not a list, passed to "
                  (describe-object applicative)))
          context)
        (simple-map
          (lambda (operand) (eval operand env context))
          operand-tree))))

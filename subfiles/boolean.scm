; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

(set-version (list 0.0 0)
             (list 0.1 0))
(set-revision-date 2007 8 5)

;;;;;;;;;;;;
; booleans ;
;;;;;;;;;;;;

;
; Given zero or more boolean arguments, returns their conjunction (i.e.,
; returns #t unless at least one of them is false).
;
(define and?
  (lambda ls
    (or (null? ls)
        (and (car ls)
             (apply and? (cdr ls))))))

;
; Creates bindings for handling booleans in a given environment.
;
(define bind-boolean-primitives!
  (lambda (env)
    (add-bindings! env

      'boolean?
      (unary-predicate->applicative boolean?)

    ; 'and?
    ; (action->checked-applicative
    ;   (lambda (operand-tree env context)
    ;     (apply and? (kernel-list->list operand-tree)))
    ;   0 -1 boolean?)

      '$if
      (action->checked-operative
        (lambda (operand-tree env context)
          (let ((test  (eval (kernel-car operand-tree) env context)))
            (if (boolean? test)
                (if test
                    (eval (kernel-cadr operand-tree) env context)
                    (eval (kernel-caddr operand-tree) env context))
                (error-pass
                  (make-error-descriptor
                    "Non-boolean test result, when calling #[operative $if]")
                  context))))
          3 3)

      )))

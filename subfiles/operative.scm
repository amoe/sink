; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

;;;;;;;;;;;;;;
; operatives ;
;;;;;;;;;;;;;;
;
; An operative has type 'operative, and attribute 'action whose value is a
; procedure whose arguments are the operand tree, environment, and context.
;

(library (subfiles operative)
  (export)
  (import (rnrs)
          (subfiles object)
          (subfiles context)
          (subfiles error)
          (subfiles revision))

(define action->operative
  (lambda (action)
    (let ((name  (list #t)))
      (lambda (message)
        (case message
          ((type)   'operative)
          ((name)   name)
          ((action) action))))))

; XXX
(define operative? (make-object-type-predicate 'operative))

;
; Calls an operative.
;
(define operate
  (lambda (operative operand-tree env context)
    ((operative 'action) operand-tree env context)))

;
; Checks a (kernel) operand list, given its min and max allowed lengths, and
; optionally predicates with which to type-check the individual operands.
; Arguments:
;     operands   --- the unchecked operand tree.
;     min        --- the minimum allowed number of operands.
;     max        --- the maximum allowed number of operands, or negative
;                   if there is no maximum.
;   . predicates --- the predicates, if any.
; If there are no predicates, only the number of operands is checked.
; If there is just one predicate, it is used on all the operands.
; If there is more than one predicate, each predicate is used on just one
; operand, left-to-right, until either there are no more operands or there is
; just one more predicate; when there is just one more predicate, it is used
; on any remaining operands.
;
; The result is an error message string if an error was detected; otherwise,
; the result is the list metrics of the operand list.
;

; XXX: GET-LIST-METRICS
;; (define check-operand-list
;;   (lambda (operands min max . predicates)

;;     (define aux
;;       (lambda (p k operands . predicates)
;;         (cond ((<= k 0)  ())
;;               (((car predicates) (kernel-car operands))
;;                  (apply aux p
;;                             (- k 1)
;;                             (kernel-cdr operands)
;;                             (if (null? (cdr predicates))
;;                                 predicates
;;                                 (cdr predicates))))
;;               (else
;;                 (string-append
;;                   "Operand #" (number->string (- p k -1))
;;                   " has wrong type")))))

;;     (let* ((metrics  (get-list-metrics operands))
;;            (p  (car metrics))
;;            (n  (cadr metrics))
;;            (c  (cadddr metrics)))
;;       (cond ((and (= n 0) (= c 0))  "Operand tree is not a list")
;;             ((and (>= max 0)
;;                   (or (> p max) (> c 0)))
;;                (string-append
;;                  "Too many operands (more than " (number->string max) ")"))
;;             ((< p min)
;;                (string-append
;;                  "Not enough operands (fewer than " (number->string min) ")"))
;;             ((null? predicates)
;;                metrics)
;;             (else
;;                (let ((emsg  (apply aux p p operands predicates)))
;;                  (if (string? emsg)
;;                      emsg
;;                      metrics)))))))

;
; Given a "naive action", returns an action that does the same thing.
; A string naming the action is also given, to be used if the naive action
; doesn't return normally.
;
; Here, a "naive action" is a Scheme procedure that takes as its one argument
; an operand tree, and either returns a result or causes a Scheme error.
; (Recall that an action takes three arguments:  operand-tree, environment,
; and context.)
;
; The constructed action ignores its second and third arguments (environment
; and context), passes its first argument to the naive action, and returns the
; normal result of the naive action.  If a Scheme error occurs during the call
; to the naive action, the full action attempts to intercept the Scheme error
; (by means of dynamic-wind) and throw a Kernel error in its place, using the
; provided error-descriptor-line.
;
; There isn't anything in the R5RS that requires dynamic-wind to intercept
; error-signals, although it does in MzScheme (version 103); if dynamic-wind
; doesn't intercept error-signals, then errors in the naive action will crash
; the Kernel interpreter.
;
; XXX: ERROR-PASS
(define naive->action
  (lambda (naive name)
    (lambda (operand-tree env context)
      (let ((completed  #f))
        (dynamic-wind
          (lambda () '())
          (lambda () (let ((result  (naive operand-tree)))
                       (set! completed #t)
                       result))
          (lambda () (if (not completed)
                         (error-pass (make-error-descriptor
                                       (list "Error when calling primitive "
                                             name))
                                     context))))))))

;
; Given an action, and criteria for admissible operand-lists for that action,
; constructs an operative that checks its operand-tree for those criteria,
; and either invokes the given action, or throws an error.
;
; The first argument is the action to be safeguarded, and the second and later
; arguments are as the second and later arguments to check-operand-list.
;
; XXX: CHECK-OPERAND-LIST (SF)
;; (define action->checked-operative
;;   (lambda (action . criteria)
;;     (letrec ((this  (action->operative
;;                       (lambda (operand-tree env context)
;;                         (let ((result  (apply check-operand-list
;;                                               operand-tree criteria)))
;;                           (if (string? result)
;;                               (error-pass
;;                                 (make-error-descriptor
;;                                   (list result " when calling "
;;                                         (describe-object this))
;;                                   (list "Operand tree: " (list operand-tree)))
;;                                 context)
;;                               (action operand-tree env context)))))))
;;       this)))

;
; metered-action->checked-operative
;
; As action->checked-operative, except that the first argument passed to the
; action is the cons of the list metrics of the operand tree with the operand
; tree.  (That is, the car of the first argument is the list metrics of the
; tree, and the cdr of the first argument is the operand tree.)  This allows
; the action to know the shape of the operand tree without a redundant call to
; get-list-metrics.
;
; XXX: CHECK-OPERAND-LIST (SF)
;; (define metered-action->checked-operative
;;   (lambda (action . criteria)
;;     (letrec ((this  (action->operative
;;                       (lambda (operand-tree env context)
;;                         (let ((result  (apply check-operand-list
;;                                               operand-tree criteria)))
;;                           (if (string? result)
;;                               (error-pass
;;                                 (make-error-descriptor
;;                                   (list result " when calling "
;;                                         (describe-object this))
;;                                   (list "Operand tree: " (list operand-tree)))
;;                                 context)
;;                               (action (cons result operand-tree)
;;                                       env context)))))))
;;       this)))

;
; naive->checked-operative
;
; As action->checked-operative, except that it's given a naive action and a
; name, instead of an action.
;
; This is the composition of naive->action with action->checked-operative.
;
; XXX: ACTION->CHECKED-OPERATIVE (SF)
;; (define naive->checked-operative
;;   (lambda (naive name . criteria)
;;     (apply action->checked-operative
;;            (naive->action naive name)
;;            criteria)))

;
; metered-naive->checked-operative
;
; As naive->checked-operative, except that the single argument passed to the
; naive action is the cons of the list metrics of the operand tree with the
; operand tree.  This is the composition of naive->action with
; metered-action->checked-operative.
;
; XXX: METERED-ACTION->CHECKED-OPERATIVE (SF)
;; (define metered-naive->checked-operative
;;   (lambda (naive name . criteria)
;;     (apply metered-action->checked-operative
;;            (naive->action naive name)
;;            criteria)))

;
; Given a Scheme unary predicate, returns an operative that determines whether
; the predicate returns true on all of the operands.
;
; The predicate must not throw a Scheme error.
;
; XXX: METERED-ACTION->CHECKED-OPERATIVE (SF)
;; (define unary-predicate->operative
;;   (lambda (unary)

;;     (define aux
;;       (lambda (n operands)
;;         (cond ((<= n 0)  #t)
;;               ((not (unary (kernel-car operands)))  #f)
;;               (else  (aux (- n 1) (kernel-cdr operands))))))

;;     (metered-action->checked-operative
;;       (lambda (x env context)
;;         (aux (caar x) (cdr x)))
;;       0 -1)))

;
; Given a Scheme binary predicate and a type predicate that must be satisfied
; by all arguments to the predicate, returns an operative that determines
; whether the Scheme predicate returns true on all consecutive operands (i.e.,
; the first and second, second and third, etc.).
;
; The predicate must not throw a Scheme error, but it may return an error
; message string instead of a boolean.
;
; XXX: METERED-ACTION->CHECKED-OPERATIVE (SF)
;; (define binary-predicate->operative
;;   (lambda (binary type?)
;;     (define this
;;       (metered-action->checked-operative
;;         (lambda (x env context)
;;           (let ((p  (car (car x)))
;;                 (c  (cadddr (car x)))
;;                 (operand-tree  (cdr x)))

;;             (define aux
;;               (lambda (n operands)
;;                 (if (<= n 1)
;;                     #t
;;                     (let ((result  (binary (kernel-car operands)
;;                                            (kernel-cadr operands))))
;;                       (if (string? result)
;;                           (error-pass
;;                             (make-error-descriptor
;;                               (list result " when calling primitive "
;;                                     (describe-object this))
;;                               (list "Failed comparing objects:  "
;;                                     (list (kernel-car operands)) "  "
;;                                     (list (kernel-cadr operands)))
;;                               (list "Operand tree: " (list operand-tree)))
;;                             context)
;;                           (if (not result)
;;                               #f
;;                               (aux (- n 1) (kernel-cdr operands))))))))

;;             (aux (+ p (if (> c 0) 1 0))
;;                  operand-tree)))

;;         0 -1 type?))

;;     this))

;
; Given a procedure with Scheme-style interface (that either returns a result
; or causes a Scheme error), a Scheme-style argument list to be passed to it,
; Kernel error-descriptor-line, and context, applies the procedure to the
; argument list, signaling an error on failure.
;
; Uses the same platform-dependent technique to capture Scheme errors as
; does naive->action, q.v.
;
(define apply-safely
  (lambda (proc arg-list message context)
    (let ((completed  #f))
      (dynamic-wind
        (lambda () '())
        (lambda () (let ((result  (apply proc arg-list)))
                     (set! completed #t)
                     result))
        (lambda () (if (not completed)
                       (error-pass (make-error-descriptor message)
                                   context)))))))


(set-version (list 0.1 0))
(set-revision-date 2007 8 5)

)

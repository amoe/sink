; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

(set-version (list 0.0 0)
             (list 0.1 0))
(set-revision-date 2007 8 4)

;;;;;;;;;;;;;;;;
; applicatives ;
;;;;;;;;;;;;;;;;
;
; An applicative has type 'applicative, and attribute 'underlying whose value
; is a combiner.
;   The principal constructor is called "wrap" instead of "make-applicative",
; and the accessor is called "unwrap" instead of "get-applicative-underlying".
;

(define wrap
  (lambda (combiner)
    (let ((appv  (let ((name  (list #t)))
                   (lambda (message)
                     (case message
                       ((type)       'applicative)
                       ((name)       name)
                       ((underlying) combiner))))))
      (designate-name-inheritor! appv combiner)
      appv)))

(define applicative? (make-object-type-predicate 'applicative))

(define unwrap (lambda (x) (x 'underlying)))

;
;
;
(define unary-predicate->applicative
  (lambda x
    (wrap (apply unary-predicate->operative x))))

(define binary-predicate->applicative
  (lambda x
    (wrap (apply binary-predicate->operative x))))

(define metered-action->checked-applicative
  (lambda x
    (wrap (apply metered-action->checked-operative x))))

(define naive->checked-applicative
  (lambda x
    (wrap (apply naive->checked-operative x))))

(define metered-naive->checked-applicative
  (lambda x
    (wrap (apply metered-naive->checked-operative x))))

;
; Given an action, and criteria for admissible argument-lists for that action,
; constructs an applicative that checks its argument-list for those criteria,
; and either invokes the given action, or throws an error.  Shorthand for
; composition of wrap with action->checked-operative.
;
(define action->checked-applicative
  (lambda x
    (wrap (apply action->checked-operative x))))

;
; Given an action, constructs an applicative whose underlying operative has
; that action.  Shorthand for composition of wrap with action->operative.
;
(define action->applicative
  (lambda (action)
    (wrap (action->operative action))))

;
; Creates bindings for this type in a given environment.
;
; This code should not use any internal knowledge of the applicative type.
; It appears in this file, rather than in "subfiles/ground.scm", simply
; because it is logically associated with the applicative type.
;
(define bind-applicative-primitives!
  (lambda (env)
    (add-bindings! env

      'applicative?
      (unary-predicate->applicative  applicative?)

      'wrap
      (action->checked-applicative
        (lambda (operand-tree env context)
          (wrap (kernel-car operand-tree)))
        1  1 combiner?)

      'unwrap
      (action->checked-applicative
        (lambda (operand-tree env context)
          (unwrap (kernel-car operand-tree)))
        1  1 applicative?)

      )))

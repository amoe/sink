#!r6rs
; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

(library (subfiles binding)
  (export
   bind-applicative-primitives!
   bind-boolean-primitives!
   bind-context-primitives!
   bind-cyclic-primitives!
   bind-encapsulation-primitives!
   bind-environment-primitives!
   bind-error-descriptor-primitives!
   bind-ignore-primitives!
   bind-inert-primitives!
   bind-kernel-pair-primitives!
   bind-keyed-variable-primitives!
   bind-number-primitives!
   bind-operative-primitives!
   bind-port-primitives!)
  (import (rnrs)
          (only (rnrs r5rs) quotient
                            remainder
                            modulo)
          (subfiles environment)
          (subfiles applicative)
          (subfiles inert)
          (subfiles operative)
          (subfiles eval)
          (subfiles kernel-pair)
          (subfiles proxy-1)
          (subfiles proxy-2)
          (subfiles error)
          (subfiles encapsulation)
          (subfiles object)
          (subfiles keyed)
          (subfiles context)
          (subfiles cycles)
          (subfiles port)
          (subfiles boolean)
          (subfiles number)
          (subfiles ignore))

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

;
; Creates bindings for this type in a given environment.
;
; This code should not use any internal knowledge of the error-descriptor
; type.  It appears in this file, rather than in "subfiles/ground.scm", simply
; because it is logically associated with the inert type.
;
(define bind-error-descriptor-primitives!
  (lambda (env)
    (add-bindings! env

      'error-descriptor?  (unary-predicate->applicative error-descriptor?))))


;
; Creates bindings for keyed variables in a given environment.
;
(define bind-keyed-variable-primitives!
  (lambda (env)
    (add-bindings! env

      'make-keyed-dynamic-variable
      (naive->checked-applicative
        (lambda (operand-tree)
          (let ((key  (get-fresh-key)))
            (list
              (action->checked-applicative
                (lambda (operand-list env context)
                  (call-with-keyed-context
                    (lambda (context)
                      (eval (kernel-cdr operand-list) env context))
                    context
                    key
                    (kernel-car operand-list)))
                2 2 any? combiner?)
              (action->checked-applicative
                (lambda (operand-list env context)
                  (context-keyed-lookup key context))
                0 0))))
        "make-keyed-dynamic-variable"
        0 0)

      'make-keyed-static-variable
      (naive->checked-applicative
        (lambda (operand-tree)
          (let ((key  (get-fresh-key)))
            (list
              (action->checked-applicative
                (lambda (operand-list env context)
                  (make-environment-with-keyed-binding
                    key
                    (kernel-car operand-list)
                    (kernel-cadr operand-list)))
                2 2 any? environment?)
              (action->checked-applicative
                (lambda (operand-list env context)
                  (environment-keyed-lookup key env context))
                0 0))))
        "make-keyed-static-variable"
        0 0)

      )))


;
; Creates bindings for this type in a given environment.
;
; This code should not use any internal knowledge of the environment type.
; It appears in this file, rather than in "subfiles/ground.scm", simply
; because it is logically associated with the type.
;
(define bind-environment-primitives!
  (lambda (env)
    (add-bindings! env

      'environment?
      (unary-predicate->applicative  environment?)

      'eval
      (action->checked-applicative
        (lambda (operand-tree env context)
          (eval (kernel-car operand-tree) (kernel-cadr operand-tree) context))
        2 2 any? environment?)

      'make-environment
      (naive->checked-applicative
        (lambda (operand-tree)
          (apply make-environment
                 (copy-kernel-list->list operand-tree)))
        "make-environment"
        0 -1 environment?)

      '$define!
      (action->checked-operative
        (lambda (operand-tree env context)
           (let ((ed  (match! env (kernel-car operand-tree)
                                  (eval (kernel-cadr operand-tree)
                                        env context))))
             (if (error-descriptor? ed)
                 (begin
                   (error-add-to-first-line!  ed
                     " when calling #[operative $define!]")
                   (error-pass ed context))
                 inert)))
        2 2)

      )))


;
; Creates bindings for these types in a given environment.
;
; This code should not use any internal knowledge of the kernel port types.
; It appears in this file, rather than in "subfiles/ground.scm", simply
; because it is logically associated with the kernel port types.
;
(define bind-port-primitives!
  (lambda (env)
    (add-bindings! env

      'input-port?
      (unary-predicate->applicative  kernel-input-port?)

      'output-port?
      (unary-predicate->applicative  kernel-output-port?)

      'open-input-file
      (action->checked-applicative
        (lambda (operand-tree env context)
          (open-kernel-input-file (kernel-car operand-tree) context))
        1 1 string?)

      'open-output-file
      (action->checked-applicative
        (lambda (operand-tree env context)
          (open-kernel-output-file (kernel-car operand-tree) context))
        1 1 string?)

      'close-input-port
      (action->checked-applicative
        (lambda (operand-tree env context)
          (close-kernel-input-port (kernel-car operand-tree) context)
          inert)
        1 1 kernel-input-port?)

      'close-output-port
      (action->checked-applicative
        (lambda (operand-tree env context)
          (close-kernel-output-port (kernel-car operand-tree) context)
          inert)
        1 1 kernel-output-port?)

      'read
      (action->checked-applicative
        (lambda (operand-tree env context)
          (kernel-read
            (if (null? operand-tree)
                (get-kernel-current-input-port context)
                (kernel-car operand-tree))
            context))
        0 1 kernel-input-port?)

      'read-char
      (action->checked-applicative
        (lambda (operand-tree env context)
          (kernel-read-char
            (if (null? operand-tree)
                (get-kernel-current-input-port context)
                (kernel-car operand-tree))
            context))
        0 1 kernel-input-port?)

      'peek-char
      (action->checked-applicative
        (lambda (operand-tree env context)
          (kernel-peek-char
            (if (null? operand-tree)
                (get-kernel-current-input-port context)
                (kernel-car operand-tree))
            context))
        0 1 kernel-input-port?)

      'write
      (action->checked-applicative
        (lambda (operand-tree env context)
          (kernel-write
            (kernel-car operand-tree)
            (if (null? (kernel-cdr operand-tree))
                (get-kernel-current-output-port context)
                (kernel-cadr operand-tree))
            context)
          inert)
        1 2 any? kernel-output-port?)

      'display
      (action->checked-applicative
        (lambda (operand-tree env context)
          (kernel-display
            (kernel-car operand-tree)
            (if (null? (kernel-cdr operand-tree))
                (get-kernel-current-output-port context)
                (kernel-cadr operand-tree))
            context)
          inert)
        1 2 any? kernel-output-port?)

      'newline
      (action->checked-applicative
        (lambda (operand-tree env context)
          (kernel-newline
            (if (null? operand-tree)
                (get-kernel-current-output-port context)
                (kernel-car operand-tree))
            context)
          inert)
        0 1 kernel-output-port?)

      'write-char
      (action->checked-applicative
        (lambda (operand-tree env context)
          (kernel-write-char
            (kernel-car operand-tree)
            (if (null? (kernel-cdr operand-tree))
                (get-kernel-current-output-port context)
                (kernel-cadr operand-tree))
            context)
          inert)
        1 2 char? kernel-output-port?)

      'get-current-input-port
      (action->checked-applicative
        (lambda (operand-tree env context)
          (get-kernel-current-input-port context))
        0 0)

      'get-current-output-port
      (action->checked-applicative
        (lambda (operand-tree env context)
          (get-kernel-current-output-port context))
        0 0)

      'with-input-from-file
      (action->checked-applicative
        (lambda (operand-tree env context)
          (let* ((name      (kernel-car operand-tree))
                 (combiner  (kernel-cadr operand-tree))
                 (kip       (open-kernel-input-file name context))
                 (result    (call-with-input-context
                              (lambda (context)
                                (combine combiner '() env context))
                              context
                              kip)))
            (close-kernel-input-port kip context)
            result))
        2 2 string? combiner?)

      'with-output-to-file
      (action->checked-applicative
        (lambda (operand-tree env context)
          (let* ((name      (kernel-car operand-tree))
                 (combiner  (kernel-cadr operand-tree))
                 (kop       (open-kernel-output-file name context))
                 (result    (call-with-output-context
                              (lambda (context)
                                (combine combiner '() env context))
                              context
                              kop)))
            (close-kernel-output-port kop context)
            result))
        2 2 string? combiner?)

      )))

;
; Creates bindings for this type in a given environment.
;
; This code should not use any internal knowledge of the operative type.
; It appears in this file, rather than in "subfiles/ground.scm", simply because
; it is logically associated with the operative type.
;
(define bind-operative-primitives!
  (lambda (env)
    (add-bindings! env

      'operative?  (unary-predicate->applicative operative?)

      '$vau
      (action->checked-operative
        (lambda (operand-tree static-env context)
          (let ((parameter-tree  (copy-es-immutable
                                   (kernel-car  operand-tree)))
                (env-parameter   (kernel-cadr operand-tree))
                (body            (copy-es-immutable
                                   (kernel-cddr operand-tree))))
            (letrec ((this
              (action->operative
                (lambda (operand-tree dynamic-env context)
                  (let ((local-env  (make-environment static-env)))
                    (suggest-object-name local-env this)
                    (let ((ed  (match! local-env
                                 (kernel-cons parameter-tree env-parameter)
                                 (kernel-cons operand-tree   dynamic-env))))
                      (if (error-descriptor? ed)
                          (begin
                            (error-add-to-first-line!  ed
                              " when calling "
                              (describe-object this))
                            (error-pass ed context))
                          (eval-sequence body local-env context))))))))
                   this)))
        3 3)
        ; changing these numbers from "3 3" to "2 -1" enables compound bodies

      )))


;
; Creates bindings for this type in a given environment.
;
; This code should not use any internal knowledge of the context type.
; It appears in this file, rather than in "subfiles/ground.scm", simply
; because it is logically associated with the context type.
;
(define bind-context-primitives!
  (lambda (env)

    (define guards-list?
      (lambda (x)
        (and (kernel-list? x)
             (apply and?
                    (map (lambda (x)
                           (and (kernel-pair? x)
                                (context? (kernel-car x))
                                (kernel-pair? (kernel-cdr x))
                                (applicative? (kernel-cadr x))
                                (operative? (unwrap (kernel-cadr x)))
                                (null? (kernel-cddr x))))
                         (kernel-list->list x))))))

    (add-bindings! env

      'continuation?
      (unary-predicate->applicative  context?)

      'call/cc
      (action->checked-applicative
        (lambda (operand-tree env context)
          (call-with-guarded-context
            (lambda (context)
              (eval (kernel-list (kernel-car operand-tree) context)
                    env context))
            context
            '()
            '()))
        1 1 combiner?)

      'extend-continuation
      (action->checked-applicative
        (lambda (operand-tree env context)
          (let ((parent  (kernel-car  operand-tree))
                (appv    (kernel-cadr operand-tree))
                (env     (if (kernel-pair? (kernel-cddr operand-tree))
                             (kernel-caddr operand-tree)
                             (make-environment))))
            (call-with-current-continuation
              (lambda (c)
                (let ((operand-tree
                        (call-with-current-continuation
                          (lambda (receiver)
                            (let ((error-context    (parent 'error-context))
                                  (terminal-context (parent 'terminal-context))
                                  (alist            (parent 'alist)))
                              (c (make-context receiver parent '() '()
                                   error-context terminal-context alist)))))))
                  ((parent 'receiver)
                   (eval (kernel-cons (unwrap appv) operand-tree)
                         env parent)))))))
        2 3 context? applicative? environment?)

      'guard-continuation
      (action->checked-applicative
        (lambda (operand-tree env context)
          (let* ((divert  '())
                 (convert-clause
                   (lambda (clause)
                     (let ((selector     (kernel-car clause))
                           (interceptor  (unwrap (kernel-cadr clause))))
                       (cons selector
                             (lambda (x)
                               (eval (kernel-list
                                       interceptor
                                       x (context->applicative divert))
                                     env divert))))))
                 (entry-guards  (map convert-clause
                                     (kernel-list->list
                                       (kernel-car operand-tree))))
                 (parent        (kernel-cadr operand-tree))
                 (exit-guards   (map convert-clause
                                     (kernel-list->list
                                       (kernel-caddr operand-tree)))))
             (call-with-current-continuation
               (lambda (c)
                 (let ((operand-tree
                         (call-with-guarded-context
                           (lambda (outer-context)
                             (call-with-guarded-context
                               (lambda (inner-context)
                                 (set! divert outer-context)
                                 (c inner-context))
                               outer-context
                               '()
                               exit-guards))
                           parent
                           entry-guards
                           '())))
                   ((parent 'receiver)
                    operand-tree))))))
        3 3 guards-list? context? guards-list?)

      'continuation->applicative
      (action->checked-applicative
        (lambda (operand-tree env context)
          (context->applicative (kernel-car operand-tree)))
        1 1 context?)

      )))


;
; Creates bindings for this type in a given environment.
;
; This code should not use any internal knowledge of the kernel-pair type.
; It appears in this file, rather than in "subfiles/ground.scm", simply
; because it is logically associated with the kernel-pair type.
;
(define bind-kernel-pair-primitives!
  (lambda (env)
    (add-bindings! env

      'pair? (unary-predicate->applicative  kernel-pair?)

      'cons
      (naive->checked-applicative
        (lambda (operand-tree)
          (kernel-cons (kernel-car operand-tree)
                       (kernel-cadr operand-tree)))
        "cons"
        2 2)

      'copy-es-immutable
      (naive->checked-applicative
        (lambda (operand-tree)
          (copy-es-immutable (kernel-car operand-tree)))
        "copy-es-immutable"
        1 1)

      'copy-es
      (naive->checked-applicative
        (lambda (operand-tree)
          (copy-es (kernel-car operand-tree)))
        "copy-es"
        1 1)

      'set-car!
      (action->checked-applicative
        (lambda (operand-tree env context)
          (let ((x  (kernel-car operand-tree))
                (y  (kernel-cadr operand-tree)))
            (if (mutable? x)
                (kernel-set-car! x y)
                (error-pass (make-error-descriptor
                              (list "Operand #1 is immutable"
                                    " when calling primitive set-car!")
                              (list "Operand tree: " (list operand-tree)))
                            context)))
          inert)
        2 2 kernel-pair? any?)

      'set-cdr!
      (action->checked-applicative
        (lambda (operand-tree env context)
          (let ((x  (kernel-car operand-tree))
                (y  (kernel-cadr operand-tree)))
            (if (mutable? x)
                (kernel-set-cdr! x y)
                (error-pass (make-error-descriptor
                              (list "Operand #1 is immutable"
                                    " when calling primitive set-cdr!")
                              (list "Operand tree: " (list operand-tree)))
                            context)))
          inert)
        2 2 kernel-pair? any?)

      )))


;
; Creates bindings for this type in a given environment.
;
; If there were any internal knowledge of the Kernel number type, this code
; would ought not to use it.  The file currently exists solely as a place to
; put this code, and if there were more to the type, the code would still
; appear here simply because it is logically associated with the type.
;
(define bind-number-primitives!
  (lambda (env)
    (add-bindings! env

      'number?   (unary-predicate->applicative kernel-number?)
      'real?     (unary-predicate->applicative kernel-real?)
      'rational? (unary-predicate->applicative rational?)
      'integer?  (unary-predicate->applicative integer?)
      'exact?    (unary-predicate->applicative kernel-exact?)
      'inexact?  (unary-predicate->applicative
                    (lambda (x) (not (kernel-exact? x))))

      '+
      (letrec ((this
                 (action->checked-applicative
                   (lambda (operand-tree env context)
                     (let ((result  (kernel-add operand-tree)))
                       (if (string? result)
                           (error-pass
                             (make-error-descriptor
                               (list result ", " "when calling "
                                     (describe-object this))
                               (list "Operand tree: " operand-tree))
                             context)
                           result)))
                   0 -1 kernel-number?)))
        this)

      '-
      (letrec ((this
                 (action->checked-applicative
                   (lambda (operand-tree env context)
                     (let ((result  (kernel-add (kernel-cdr operand-tree))))
                       (if (not (string? result))
                           (set! result (kernel-binary-add
                                          (kernel-car operand-tree)
                                          (kernel-negate result))))
                       (if (string? result)
                           (error-pass
                             (make-error-descriptor
                               (list result ", " "when calling"
                                     (describe-object this))
                               (list "Operand-tree: " operand-tree))
                             context)
                           result)))
                   2 -1 kernel-number?)))
        this)

      '*
      (letrec ((this
                 (action->checked-applicative
                   (lambda (operand-tree env context)
                     (let ((result  (kernel-multiply operand-tree)))
                       (if (string? result)
                           (error-pass
                             (make-error-descriptor
                               (list result ", " "when calling "
                                     (describe-object this))
                               (list "Operand tree: " operand-tree))
                             context)
                           result)))
                   0 -1 kernel-number?)))
        this)

      '/
      (letrec ((this
                 (action->checked-applicative
                   (lambda (operand-tree env context)
                     (let ((result  (kernel-binary-multiply
                                      (kernel-car operand-tree)
                                      (kernel-invert
                                        (kernel-multiply
                                          (kernel-cdr operand-tree))))))
                       (if (string? result)
                           (error-pass
                             (make-error-descriptor
                               (list result ", " "when calling "
                                     (describe-object this))
                               (list "Operand tree: " operand-tree))
                             context)
                           result)))
                   2 -1 kernel-number?)))
        this)

      '<?  (binary-predicate->applicative  kernel-<?   kernel-real?)
      '<=? (binary-predicate->applicative  kernel-<=?  kernel-real?)
      '=?  (binary-predicate->applicative  kernel-=?   kernel-real?)
      '>?  (binary-predicate->applicative  kernel->?   kernel-real?)
      '>=? (binary-predicate->applicative  kernel->=?  kernel-real?)

      'round
      (naive->checked-applicative
        (lambda (operand-tree)
          (round (kernel-car operand-tree)))
        "round"
        1 1 (lambda (x) (and (kernel-real? x) (not (infinity? x)))))

      'floor
      (naive->checked-applicative
        (lambda (operand-tree)
          (floor (kernel-car operand-tree)))
        "floor"
        1 1 (lambda (x) (and (kernel-real? x) (not (infinity? x)))))

      'ceiling
      (naive->checked-applicative
        (lambda (operand-tree)
          (ceiling (kernel-car operand-tree)))
        "ceiling"
        1 1 (lambda (x) (and (kernel-real? x) (not (infinity? x)))))

      'quotient
      (naive->checked-applicative
        (lambda (operand-tree)
          (quotient (kernel-car operand-tree)
                    (kernel-cadr operand-tree)))
        "quotient"
        2 2 integer? (lambda (x) (and (integer? x) (not (zero? x)))))

      'remainder
      (naive->checked-applicative
        (lambda (operand-tree)
          (remainder (kernel-car operand-tree)
                     (kernel-cadr operand-tree)))
        "remainder"
        2 2 integer? (lambda (x) (and (integer? x) (not (zero? x)))))

      'modulo
      (naive->checked-applicative
        (lambda (operand-tree)
          (modulo (kernel-car operand-tree)
                  (kernel-cadr operand-tree)))
        "modulo"
        2 2 integer? (lambda (x) (and (integer? x) (not (zero? x)))))

      'exact->inexact
      (naive->checked-applicative
        (lambda (operand-tree)
          (let ((x  (kernel-car operand-tree)))
            (cond ((not (kernel-exact? x))  x)
                  ((exact-positive-infinity? x)  inexact-positive-infinity)
                  ((exact-negative-infinity? x)  inexact-negative-infinity)
                  (else  (inexact x)))))
        "exact->inexact"
        1 1 kernel-real?)

      'inexact->exact
      (naive->checked-applicative
        (lambda (operand-tree)
          (let ((x  (kernel-car operand-tree)))
            (cond ((kernel-exact? x)  x)
                  ((inexact-positive-infinity? x)  exact-positive-infinity)
                  ((inexact-negative-infinity? x)  exact-negative-infinity)
                  (else  (exact x)))))
        "inexact->exact"
        1 1 kernel-real?)

      'log
      (naive->checked-applicative
        (lambda (operand-tree)
          (let ((x  (kernel-car operand-tree)))
            (cond ((positive-infinity? x)  x)
                  ((zero? x)  (if (exact? x)
                                  exact-negative-infinity
                                  inexact-negative-infinity))
                  (else  (log x)))))
        "log"
        1 1 (lambda (x) (and (kernel-real? x) (kernel-<? 0 x))))

      'sqrt
      (naive->checked-applicative
        (lambda (operand-tree)
          (let ((x  (kernel-car operand-tree)))
            (cond ((positive-infinity? x)  x)
                  (else  (sqrt x)))))
        "sqrt"
        1 1 (lambda (x) (and (kernel-real? x) (kernel-<=? 0 x))))

      'sin
      (naive->checked-applicative
        (lambda (operand-tree)
          (sin (kernel-car operand-tree)))
        "sin"
        1 1 (lambda (x) (and (kernel-real? x) (not (infinity? x)))))

      'cos
      (naive->checked-applicative
        (lambda (operand-tree)
          (cos (kernel-car operand-tree)))
        "cos"
        1 1 (lambda (x) (and (kernel-real? x) (not (infinity? x)))))

      'tan
      (naive->checked-applicative
        (lambda (operand-tree)
          (tan (kernel-car operand-tree)))
        "tan"
        1 1 (lambda (x) (and (kernel-real? x) (not (infinity? x)))))

      'asin
      (naive->checked-applicative
        (lambda (operand-tree)
          (asin (kernel-car operand-tree)))
        "asin"
        1 1 (lambda (x) (and (kernel-real? x) (<= -1 x 1))))

      'acos
      (naive->checked-applicative
        (lambda (operand-tree)
          (acos (kernel-car operand-tree)))
        "acos"
        1 1 (lambda (x) (and (kernel-real? x) (<= -1 x 1))))

      'atan
      (letrec ((this
                 (action->checked-applicative
                   (lambda (operand-tree env context)
                     (if (kernel-pair? (kernel-cdr operand-tree))
                       (let ((x  (kernel-car operand-tree))
                             (y  (kernel-cadr operand-tree)))
                         (cond ((or (and (infinity? x) (infinity? y))
                                    (and (kernel-zero? x) (kernel-zero? y)))
                                  (error-pass
                                    (make-error-descriptor
                                      (list "indeterminate result when calling"
                                            (describe-object this))
                                      (list "Operand tree: " operand-tree))
                                    context))
                               ((positive-infinity? x)          (atan 1 0))
                               ((negative-infinity? x)          (atan -1 0))
                               ((exact-positive-infinity? y)    0)
                               ((inexact-positive-infinity? y)  0.0)
                               ((negative-infinity? y)          (atan 0 -1))
                               (else  (atan x y))))
                       (let ((x  (kernel-car operand-tree)))
                         (cond ((positive-infinity? x)  (atan 1 0))
                               ((negative-infinity? x)  (atan -1 0))
                               (else  (atan x))))))
                   1 2 kernel-real?)))
        this)

      )))

;
; Creates bindings for handling cyclic structures in a given environment.
;
(define bind-cyclic-primitives!
  (lambda (env)
    (add-bindings! env

    ; 'get-list-metrics
    ; (action->checked-applicative
    ;   (lambda (operand-tree env context)
    ;     (list->kernel-list (get-list-metrics (kernel-car operand-tree))))
    ;   1 1)

    ; 'finite-list?
    ; (unary-predicate->applicative finite-list?)

    ; 'countable-list?
    ; (unary-predicate->applicative kernel-list?)

    ; 'member?
    ; (action->checked-applicative
    ;   (lambda (operand-tree env context)
    ;     (let* ((object (kernel-car operand-tree))
    ;            (ls     (kernel-cadr operand-tree))
    ;            (p      (car (get-list-metrics ls))))
    ;       (letrec ((aux?  (lambda (k ls)
    ;                         (if (<= k 0)
    ;                             #f
    ;                             (or (kernel-equal? object (kernel-car ls))
    ;                                 (aux? (- k 1) (kernel-cdr ls)))))))
    ;         (aux? p ls))))
    ;   2 2 any? kernel-list?)

    ; 'memq?
    ; (action->checked-applicative
    ;   (lambda (operand-tree env context)
    ;     (let* ((object (kernel-car operand-tree))
    ;            (ls     (kernel-cadr operand-tree))
    ;            (p      (car (get-list-metrics ls))))
    ;       (letrec ((aux?  (lambda (k ls)
    ;                         (if (<= k 0)
    ;                             #f
    ;                             (or (eq? object (kernel-car ls))
    ;                                 (aux? (- k 1) (kernel-cdr ls)))))))
    ;         (aux? p ls))))
    ;   2 2 any? kernel-list?)

    ; 'list-tail
    ; (action->checked-applicative
    ;   (lambda (operand-tree env context)
    ;     (let* ((ls  (kernel-car operand-tree))
    ;            (k   (kernel-cadr operand-tree))
    ;            (p   (car (get-list-metrics ls))))
    ;       (if (< p k)
    ;           (error-pass (make-error-descriptor
    ;                         (list "List isn't long enough"
    ;                               " when calling #[operative list-tail]")
    ;                         (list "Operand tree: " (list operand-tree)))
    ;                       context)
    ;           (kernel-list-tail ls k))))
    ;   2 2 kernel-list? integer?)

    ; 'list-ref
    ; (action->checked-applicative
    ;   (lambda (operand-tree env context)
    ;     (let* ((ls  (kernel-car operand-tree))
    ;            (k   (kernel-cadr operand-tree))
    ;            (p   (car (get-list-metrics ls))))
    ;       (if (<= p k)
    ;           (error-pass (make-error-descriptor
    ;                         (list "List isn't long enough"
    ;                               " when calling #[operative list-ref]")
    ;                         (list "Operand tree: " (list operand-tree)))
    ;                       context)
    ;           (kernel-list-ref ls k))))
    ;   2 2 kernel-list? integer?)

    ; 'encycle!
    ; (action->checked-applicative
    ;   (lambda (operand-tree env context)
    ;     (let* ((ls  (kernel-car operand-tree))
    ;            (a   (kernel-cadr operand-tree))
    ;            (c   (kernel-caddr operand-tree))
    ;            (p   (car (get-list-metrics ls))))
    ;       (cond ((< c 1)  ())
    ;             ((< p (+ a c))
    ;                (error-pass (make-error-descriptor
    ;                              (list "List isn't long enough"
    ;                                    " when calling #[operative encycle!]")
    ;                              (list "Operand tree: " (list operand-tree)))
    ;                            context))
    ;             ((immutable? (kernel-list-tail ls (+ a c -1)))
    ;                (error-pass (make-error-descriptor
    ;                              (list "Target is immutable"
    ;                                    " when calling #[operative encycle!]")
    ;                              (list "Operand tree: " (list operand-tree)))
    ;                            context))
    ;             (else  (kernel-encycle! ls a c))))
    ;     inert)
    ;   3 3 any? integer? integer?)

    ; 'map
    ; (action->checked-applicative
    ;   (lambda (operand-tree env context)
    ;     (let ((combiner  (unwrap (kernel-car operand-tree)))
    ;           (lss       (kernel-cdr operand-tree)))
    ;       (let ((result  (full-map (lambda (args)
    ;                                  (eval (kernel-cons combiner args)
    ;                                        env context))
    ;                                lss)))
    ;         (if (string? result)
    ;             (error-pass
    ;               (make-error-descriptor
    ;                 (list result ", when calling #[operative map]"))
    ;               context)
    ;             result))))
    ;   2 -1 applicative? kernel-list?)

    ;;;;;;;;;;;; doesn't look right
    ; 'append
    ; (action->checked-applicative
    ;   (lambda (operand-tree env context)
    ;
    ;     (define mutable-finite-list?
    ;       (lambda (ls)
    ;         (let* ((metrics  (get-list-metrics ls))
    ;                (p        (car metrics))
    ;                (c        (cadddr metrics)))
    ;           (and (zero? c)
    ;                (or (zero? p)
    ;                    (mutable? (kernel-list-tail ls (- p 1))))))))
    ;
    ;     (define check-operands
    ;       (lambda (n k operands)
    ;         (if (>= n k)
    ;             (if (mutable-finite-list? (kernel-car operands))
    ;                 (check-operands n (+ k 1) (kernel-cdr operands))
    ;                 (error-pass
    ;                   (make-error-descriptor
    ;                     (string-append "Operand #" (number->string k)
    ;                       (if (finite-list? (kernel-car operands))
    ;                           " is immutable"
    ;                           " has wrong type")
    ;                       " when calling #[operative append]")
    ;                     (list "Operand tree: " (list operand-tree)))
    ;                   context)))))
    ;
    ;     (define binary-append
    ;       (lambda (x y)
    ;         (if (null? x)
    ;             y
    ;             (cons (kernel-car x)
    ;                   (binary-append (kernel-cdr x) y)))))
    ;
    ;     (define bounded-append
    ;       (lambda (k lss)
    ;         (if (<= k 0)
    ;             ()
    ;             (binary-append (kernel-car lss)
    ;                            (bounded-append (- k 1) (kernel-cdr lss))))))
    ;
    ;     (define finite-append
    ;       (lambda (lss)
    ;         (if (null? lss)
    ;             ()
    ;             (let ((ls   (kernel-car lss))
    ;                   (lss  (kernel-cdr lss)))
    ;               (if (null? lss)
    ;                   ls
    ;                   (binary-append ls (finite-append lss)))))))
    ;
    ;     (define set-last!
    ;       (lambda (x y)
    ;         (if (null? (kernel-cdr x))
    ;             (kernel-set-cdr! x y)
    ;             (set-last! (kernel-cdr x) y))))
    ;
    ;     (let* ((metrics  (get-list-metrics operand-tree))
    ;            (p        (car metrics))
    ;            (a        (caddr metrics))
    ;            (c        (cadddr metrics)))
    ;       (if (zero? c)
    ;           (begin
    ;             (check-operands (- p 1) 1 operand-tree)
    ;             (finite-append operand-tree))
    ;           (begin
    ;             (check-operands p 1 operand-tree)
    ;             (let ((cycle  (bounded-append c (kernel-list-tail
    ;                                               operand-tree a))))
    ;               (set-last! cycle cycle)
    ;               (if (zero? a)
    ;                   cycle
    ;                   (let ((acyclic-prefix
    ;                           (bounded-append a operand-tree)))
    ;                     (set-last! acyclic-prefix cycle)
    ;                     acyclic-prefix)))))))
    ;   0 -1)
    ;;;;;;;;;;;;

      'equal?  (binary-predicate->applicative  kernel-equal?  any?)

      )))


;
; Creates bindings for this type in a given environment.
;
; This code should not use any internal knowledge of the ignore type (not
; that there's anything much to use).  It appears in this file, rather than
; in "subfiles/ground.scm", simply because it is logically associated with
; the ignore type.
;
(define bind-ignore-primitives!
  (lambda (env)
    (add-bindings! env

      'ignore?  (unary-predicate->applicative ignore?))))


)

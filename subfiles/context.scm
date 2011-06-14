; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

(set-version (list 0.0 1)
             (list 0.1 1))
(set-revision-date 2009 9 7)

;;;;;;;;;;;;
; contexts ;
;;;;;;;;;;;;
;
; The name "context" is used within the interpreter for what the Kernel
; programmer calls a "continuation".  The word "continuation" is used for
; Scheme continuations.
;
; A context has type 'continuation (because that's the name that should be
; visible within Kernel), and attributes 'receiver, 'parent, 'entry-guards,
; 'exit-guards, 'error-context, 'terminal-context, 'alist, and 'mark.  None
; of the attributes is directly accessible to clients.
;
;   'receiver is a continuation.  When an abnormal pass arrives at its
; destination, the abnormally passed value goes to the destination's receiver.
;
;   'parent is either nil or a context.  The parent (even if nil) contains
; this context, or equivalently, is an ancestor of this context.  Ancestry
; and containment are improper unless otherwise stated, i.e., a context is
; an ancestor of/contains itself.
;
;   'entry-guards and 'exit-guards are each a list of context/procedure pairs.
; Each pair is called a "guard", its context a "selector", and its procedure
; an "interceptor".
;   When an abnormal pass is scheduled, a list is made of interceptors to be
; called along the way.  At most one exit interceptor is selected from each
; context to be exited, in the order they will be exited, and then at most one
; entry interceptor from each context to be entered, in the order they will be
; entered.  For each exited context, the first exit guard is selected whose
; selector contains the destination of the abnormal pass; for each entered
; context, the first entry guard is selected whose selector contains the source
; of the abnormal pass.
;   Once the interceptors for the abnormal pass have been selected, they are
; used in series to transform the abnormally passed value; i.e., the value is
; passed as an argument to the first interceptor, the output of the first is
; passed as an argument to the second, etc.  The output of the last interceptor
; goes to the destination's receiver.
;
;   'error-context is a context.  When an error occurs, a descriptor of the
; error is abnormally passed to the error-context.
;
;   'terminal-context is a context.  Abnormally passing any value to the
; terminal-context requests termination of the interpreter.
;
;   'alist is a list of keyed-bindings, constructed by tools in file
; "subfiles/keyed.scm".
; 
;   'mark is a pair, unique to this context, whose car is a boolean.  Its
; car is #f except during ancestry-determination algorithms.  A context
; whose mark's car is #t is said to be "marked".  Marking contexts allows
; ancestry-determination algorithms to run in linear rather than quadratic
; time.
;

(define make-context
  (lambda (receiver parent entry-guards exit-guards
                    error-context terminal-context alist)
    (let ((name  (list #t))
          (mark  (list #f)))
      (lambda (message)
        (case message
          ((type)             'continuation)
          ((name)             name)
          ((receiver)         receiver)
          ((parent)           parent)
          ((entry-guards)     entry-guards)
          ((exit-guards)      exit-guards)
          ((mark)             mark)
          ((error-context)    error-context)
          ((terminal-context) terminal-context)
          ((alist)            alist))))))

(define context? (make-object-type-predicate 'continuation))

;
; A call to make-top-level-context may return multiple times.  The first time,
; it returns a newly allocated top-level context.  On later returns from the
; same call, it returns the same top-level context again, as long as processing
; should continue; if it returns nil, processing should terminate.
;
;   The top-level context's receiver returns the top-level context from the
; make-top-level-context call.
;
;   The top-level context's error-context's receiver passes its received
; value to the error-handling procedure (provided as an argument to
; make-top-level-context), and calls the top-level context's receiver.
;
;   The top-level context's terminal-context's receiver returns nil from the
; make-top-level-context call.
;
;   The top-level context's alist is provided by
; make-top-level-dynamic-alist.
;    
(define make-top-level-context
  (lambda (error-handler)
    (call-with-current-continuation
      (lambda (c)
        (letrec ((receiver
                   (lambda ignore (c normal-context)))
                 (alist
                   (make-top-level-dynamic-alist))
                 (terminal-context
                   (let ((delegate  (make-context
                                      (lambda ignore (c ()))
                                      () () () () () alist)))
                     (lambda (message)
                       (case message
                         ((error-context)     error-context)
                         ((terminal-context)  terminal-context)
                         (else                (delegate message))))))
                 (error-context
                   (let ((delegate  (make-context
                                      (lambda (ed)
                                        (receiver (error-handler ed)))
                                      () () () () () alist)))
                     (lambda (message)
                       (case message
                         ((parent)            terminal-context)
                         ((error-context)     error-context)
                         ((terminal-context)  terminal-context)
                         (else                (delegate message))))))
                 (normal-context
                   (let ((delegate  (make-context
                                      receiver
                                      () () () () () alist)))
                     (lambda (message)
                       (case message
                         ((parent)            terminal-context)
                         ((error-context)     error-context)
                         ((terminal-context)  terminal-context)
                         (else                (delegate message)))))))
          (receiver))))))

;
; call-with-guarded-context takes as arguments a procedure, parent context, and
; entry and exit guards; and passes as the single argument to the procedure
; a new context whose receiver is the return continuation from the call to
; call-with-guarded-context, whose parent and guards are as specified, and
; whose error-context terminal-context and alist are inherited from the parent.
;

(define call-with-guarded-context
  (lambda (proc parent entry-guards exit-guards)
    (call-with-current-continuation
      (lambda (receiver)
        (let ((error-context     (parent 'error-context))
              (terminal-context  (parent 'terminal-context))
              (alist             (parent 'alist)))
          (proc (make-context receiver parent entry-guards exit-guards
                              error-context terminal-context alist)))))))

;
; call-with-keyed-context takes as arguments a procedure, parent context, and
; key and value for a keyed binding; and passes as the single argument to
; the procedure a new context whose receiver is the return continuation from
; the call to call-with-keyed-context, whose parent is as specified, whose
; guard lists are empty, whose error-context and terminal-context are inherited
; from the parent, and whose alist is inherited from the parent except for the
; given keyed binding.
;

(define call-with-keyed-context
  (lambda (proc parent key value)
    (call-with-current-continuation
      (lambda (receiver)
        (let ((error-context     (parent 'error-context))
              (terminal-context  (parent 'terminal-context))
              (alist             (make-alist (parent 'alist) key value)))
          (proc (make-context receiver parent () ()
                              error-context terminal-context alist)))))))

;
; Given the internal key for a keyed variable, and a context, looks up the
; value of that keyed variable in that context's alist.  Returns the value
; if found, or signals an error.
;

(define context-keyed-lookup
  (lambda (key context)
    (let ((binding  (alist-lookup key (context 'alist))))
      (if (pair? binding)
          (cdr binding)
          (error-pass
            (make-error-descriptor
              "Attempted to look up an unbound keyed dynamic variable"
              (list "in " (list context)))
            context)))))

;
; Given an environment and a context, binds symbols root-continuation and
; error-continuation in the given environment to the terminal-context and
; error-context of the given context.
;
(define initialize-context-bindings
  (lambda (env context)
    (add-bindings! env 'root-continuation (context 'terminal-context)
                       'error-continuation (context 'error-context))))

;
; Given a context, constructs an applicative that abnormally passes its
; argument tree to that context.
;
(define context->applicative
  (lambda (dest-context)
    (let ((this
            (action->applicative
              (lambda (operand-tree env source-context)
                (abnormally-pass operand-tree source-context dest-context)))))
      (designate-name-inheritor! (unwrap this) dest-context)
      this)))

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
      (let ((selected  (select-entry-interceptors destination ())))
        (set-marks! source #f)
        (set-marks! destination #t)
        (let ((selected  (select-exit-interceptors source selected)))
          (set-marks! destination #f)
          ((destination 'receiver) (serial-transform selected value)))))))

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
            ()
            ()))
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
                              (c (make-context receiver parent () ()
                                   error-context terminal-context alist)))))))
                  ((parent 'receiver)
                   (eval (kernel-cons (unwrap appv) operand-tree)
                         env parent)))))))
        2 3 context? applicative? environment?)

      'guard-continuation
      (action->checked-applicative
        (lambda (operand-tree env context)
          (let* ((divert  ())
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
                               ()
                               exit-guards))
                           parent
                           entry-guards
                           ())))
                   ((parent 'receiver)
                    operand-tree))))))
        3 3 guards-list? context? guards-list?)

      'continuation->applicative
      (action->checked-applicative
        (lambda (operand-tree env context)
          (context->applicative (kernel-car operand-tree)))
        1 1 context?)

      )))

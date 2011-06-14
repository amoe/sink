; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

(set-version (list 0.0 1)
             (list 0.1 1))
(set-revision-date 2009 8 31)

;;;;;;;;;;;;;;;;;;;
; keyed variables ;
;;;;;;;;;;;;;;;;;;;
;
; Keyed variables are (from the Kernel programmer's perspective) bound and
; accessed via matched sets of combiners --- one binder and one accessor for
; each "variable".  There are two kinds of keyed variables:  dynamic keyed
; variables, which are bound in contexts, and static keyed variables, which are
; bound in environments (but are entirely separate from symbolic variables).
; Internally, each context or environment holds a list of key/value pairs; the
; holding object regulates access to the alist, but the actual operations
; are handled by tools provided here --- key assignment, alist construction,
; and lookup.
;
; The keys are integers.  Once an alist is in a context/environment, it is
; never mutated.
;
; There is no need for the alists or the keys to be encapsulated, because the
; purpose of encapsulation is to prevent Kernel programs from violating
; abstraction barriers on the objects they manipulate, and Kernel programs
; are never allowed to directly touch the alists or keys.
;

;
; Assigns a fresh key.
;
(define get-fresh-key
  (let ((counter  0))
    (lambda ()
      (set! counter (+ counter 1))
      counter)))

;
; Given an alist and a key and value, constructs a new alist whose only
; difference from the given alist is a binding of given key to given value.
;
; Allocates only as many new pairs as necessary to guarantee that the new alist
; has only one binding for the given key (assuming that the given alist didn't
; already have more than one binding for it).
;
(define make-alist
  (lambda (alist key value)

    (define aux       ; assumes the key is bound somewhere in alist
      (lambda (alist)
        (if (eq? (caar alist) key)
            (cons (cons key value) (cdr alist))
            (cons (car alist) (aux (cdr alist))))))

    (if (and (pair? alist)
             (pair? (assq key alist)))
        (aux alist)
        (cons (cons key value) alist))))

;
; Given zero or more alists, constructs a single alist containing the first
; binding for each key among the given alists.
;
(define merge-alists
  (lambda lss

    (define aux2
      (lambda (alist1 alist2)
        (if (null? alist1)
            alist2
            (make-alist (aux2 (cdr alist1) alist2)
                        (caar alist1)
                        (cdar alist1)))))

    (define aux
      (lambda (alist . lss)
        (if (null? lss)
            alist
            (aux2 alist (apply aux lss)))))

    (if (null? lss)
        lss
        (apply aux lss))))

;
; Looks up a key in an alist.
;
(define alist-lookup assq)

;
; Constructs a top-level dynamic alist.
;
; This must happen when the interpreter is called, not when the interpreter is
; constructed, because the top-level input-port and output-port should be those
; in effect when the interpreter is called, not when it is constructed.  The
; bindings are provided by procedures in other files, which are called from
; here, and which in turn are responsible for calling get-fresh-key.  The other
; procedures return alists.
;

(define make-top-level-dynamic-alist
  (lambda ()
     (make-top-level-ports-alist)))

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

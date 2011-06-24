(library (subfiles proxy-1)
  (export kernel-list->list
          kernel-read
          combiner?
          any?
          eval-sequence
          kernel-equal?)
  (import (rnrs)
          (rnrs mutable-pairs)
          (subfiles port)
          (subfiles kernel-pair)
          (subfiles ignore)
          (subfiles inert)
          (subfiles number)
          (subfiles cycles)
          (subfiles context)
          (subfiles environment)
          (subfiles operative)
          (subfiles object)
          (subfiles proxy-2)
          (subfiles error)
          (subfiles eval))

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
        (letrec* ((receiver
                   (lambda ignore (c normal-context)))
                 (alist
                   (make-top-level-dynamic-alist))
                 (terminal-context
                   (let ((delegate  (make-context
                                      (lambda ignore (c '()))
                                      '() '() '() '() '() alist)))
                     (lambda (message)
                       (case message
                         ((error-context)     error-context)
                         ((terminal-context)  terminal-context)
                         (else                (delegate message))))))
                 (error-context
                   (let ((delegate  (make-context
                                      (lambda (ed)
                                        (receiver (error-handler ed)))
                                      '() '() '() '() '() alist)))
                     (lambda (message)
                       (case message
                         ((parent)            terminal-context)
                         ((error-context)     error-context)
                         ((terminal-context)  terminal-context)
                         (else                (delegate message))))))
                 (normal-context
                   (let ((delegate  (make-context
                                      receiver
                                      '() '() '() '() '() alist)))
                     (lambda (message)
                       (case message
                         ((parent)            terminal-context)
                         ((error-context)     error-context)
                         ((terminal-context)  terminal-context)
                         (else                (delegate message)))))))
          (receiver))))))

(define make-top-level-dynamic-alist
  (lambda ()
     (make-top-level-ports-alist)))

;
; Given a scheme value presumed to have just been read, returns a mutable
; Kernel version of the value, by copying its evaluation structure and
; transforming certain symbols to their Kernel counterparts.
;
(define scheme-read-object->kernel
  (make-es-copier
    pair? car cdr
    (lambda (key)
      (let* ((kernel-pair  (kernel-cons '() '()))
             (content      (kernel-pair 'content)))
        (cons key (cons kernel-pair content))))
    kernel-cons
    (lambda (x)
      (if (symbol? x)
          (case x
            ((%ignore) ignore)
            ((%inert)  inert)
            ((%e+infinity)  exact-positive-infinity)
            ((%e-infinity)  exact-negative-infinity)
            ((%i+infinity)  inexact-positive-infinity)
            ((%i-infinity)  inexact-negative-infinity)
            (else      x))
          x))))
;
; Given two structures x and y, either of which may be cyclic, determine
; whether they are equal?.
;
; A table is maintained to keep track of which constituents (kernel-pairs) of
; x do not have to be compared again to which constituents of y.  The table
; is a list; each element of this list is a pair, whose first element is a
; constituent of x, and whose subsequent elements are constituents of y that
; don't have to be recompared to it.
;
; There is no call for this tool to use encapsulated knowledge about the
; kernel-pair type, because marking individual kernel-pairs wouldn't help.
;
(define kernel-equal?
  (lambda (x y)

    (define table '())

    (define get-row
      (lambda (x)
        (let ((row  (assq x table)))
          (if (pair? row)
              row
              (let ((row  (list x)))
                (set! table (cons row table))
                row)))))

    (define is-in-row?
      (lambda (y row)
        (if (pair? (memq y row))
            #t
            (begin
              (set-cdr! row (cons y (cdr row)))
              #f))))

    (define aux
      (lambda (x y)
        (cond ((and (kernel-pair? x) (kernel-pair? y))
                 (if (is-in-row? y (get-row x))
                     #t
                     (and (aux (kernel-car x) (kernel-car y))
                          (aux (kernel-cdr x) (kernel-cdr y)))))
              ((and (kernel-number? x) (kernel-number? y))
                 (kernel-=? x y))
              ((and (string? x) (string? y))
                 (string=? x y))
              (else
                 (eq? x y)))))

    (aux x y)))
;
; Given a kernel-list, returns a list with the same elements in the same order.
; The result is guaranteed to be a list (acyclic and made up of pairs), but is
; not guaranteed to be distinct from the given kernel-list:  if mutables are
; represented by pairs, the result may be the given kernel-list.  Therefore,
; this tool should only be used if the resultant list certainly will not be
; mutated (because mutating the result might mutate the original kernel-list).
;
; To guarantee that the result will be distinct from the argument,
; use  copy-kernel-list->list.
;
(define kernel-list->list
  (lambda (ls)
    (copy-kernel-list->list ls)))

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
; Performs i/o on a Kernel port, or signals an error.
;
(define kernel-read
  (lambda (kip context)
    (apply-safely
      (lambda (inport) (scheme-read-object->kernel (read inport)))
      (list (kip 'input-port))
      (list "Failure during read, " (list kip))
      context)))
;
; Predicates the combiner type.
;
(define combiner? (make-object-type-predicate 'operative 'applicative))

;
; Predicates anything.
;
(define any? (lambda x #t))

;
; Evaluates a sequence of expressions, and returns the last result.
; Used by both $vau and $sequence.
;
(define eval-sequence
  (lambda (operand-tree env context)
    (cond ((null? operand-tree)  inert)
          ((not (kernel-pair? operand-tree))
             (error-pass
                (make-error-descriptor
                  "Non-list operand-tree when calling #[operative $sequence]")
                context))
          ((null? (kernel-cdr operand-tree))
             (eval (kernel-car operand-tree) env context))
          (else
             (eval          (kernel-car operand-tree) env context)
             (eval-sequence (kernel-cdr operand-tree) env context)))))
)

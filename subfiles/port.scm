; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

(set-version (list 0.0 0)
             (list 0.1 0))
(set-revision-date 2007 8 4)

;;;;;;;;;;;;;;;;
; kernel ports ;
;;;;;;;;;;;;;;;;
;
; A kernel-input-port has type 'input-port, and attribute 'input-port whose
; value is a Scheme input-port.  A kernel-output-port has type 'output-port,
; and attribute 'output-port whose value is a Scheme output-port.
;

(define make-kernel-input-port
  (lambda (scheme-input-port)
    (let ((name  (list #t)))
      (lambda (message)
        (case message
          ((type)       'input-port)
          ((name)       name)
          ((input-port) scheme-input-port))))))

(define make-kernel-output-port
  (lambda (scheme-output-port)
    (let ((name  (list #t)))
      (lambda (message)
        (case message
          ((type)        'output-port)
          ((name)        name)
          ((output-port) scheme-output-port))))))

(define kernel-input-port?  (make-object-type-predicate 'input-port))
(define kernel-output-port? (make-object-type-predicate 'output-port))

;
; Opens a Kernel port, or signals an error.
;

(define open-kernel-input-file
  (lambda (name context)
    (make-kernel-input-port
      (apply-safely
        open-input-file
        (list name)
        (string-append "Cannot open file for input: \"" name "\"")
        context))))

(define open-kernel-output-file
  (lambda (name context)
    (make-kernel-output-port
      (apply-safely
        open-output-file
        (list name)
        (string-append "Cannot open file for output: \"" name "\"")
        context))))

;
; Closes a Kernel port, or signals an error.
;

(define close-kernel-input-port
  (lambda (kip context)
    (apply-safely
      close-input-port
      (list (kip 'input-port))
      (list "Cannot close " (list kip))
      context)))

(define close-kernel-output-port
  (lambda (kop context)
    (apply-safely
      close-output-port
      (list (kop 'output-port))
      (list "Cannot close " (list kop))
      context)))

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

(define kernel-read-char
  (lambda (kip context)
    (apply-safely
      read-char
      (list (kip 'input-port))
      (list "Failure during read-char, " (list kip))
      context)))

(define kernel-peek-char
  (lambda (kip context)
    (apply-safely
      peek-char
      (list (kip 'input-port))
      (list "Failure during peek-char, " (list kip))
      context)))

(define kernel-char-ready?
  (lambda (kip context)
    (apply-safely
      char-ready?
      (list (kip 'input-port))
      (list "Failure during char-ready?, " (list kip))
      context)))

(define kernel-write
  (lambda (value kop context)
    (apply-safely
      write-tree
      (list value (kop 'output-port))
      (list "Failure during write, " (list kop))
      context)))

(define kernel-display
  (lambda (value kop context)
    (apply-safely
      display-tree
      (list value (kop 'output-port))
      (list "Failure during display, " (list kop))
      context)))

(define kernel-newline
  (lambda (kop context)
    (apply-safely
      newline
      (list (kop 'output-port))
      (list "Failure during newline, " (list kop))
      context)))

(define kernel-write-char
  (lambda (char kop context)
    (apply-safely
      write-char
      (list char (kop 'output-port))
      (list "Failure during write-char, " (list kop))
      context)))

;
; Dynamic-binders, accessors, and top-level-alist constructor
; for the Kernel current-input-port and current-output-port.
;

(define get-kernel-current-input-port  ())
(define get-kernel-current-output-port ())

(define call-with-input-context  ())
(define call-with-output-context ())

(define make-top-level-ports-alist ())

(let ((make-top-level-input-port-alist  ())
      (make-top-level-output-port-alist  ()))

  (let ((kip-key  (get-fresh-key)))

    (set! get-kernel-current-input-port
      (lambda (context)
        (context-keyed-lookup kip-key context)))

    (set! call-with-input-context
      (lambda (proc parent kip)
        (call-with-keyed-context proc parent kip-key kip)))

    (set! make-top-level-input-port-alist
      (lambda ()
        (let ((kip  (make-kernel-input-port (current-input-port))))
          (suggest-object-name kip "standard-input-port")
          (make-alist
            ()
            kip-key
            kip)))))

  (let ((kop-key  (get-fresh-key)))

    (set! get-kernel-current-output-port
      (lambda (context)
        (context-keyed-lookup kop-key context)))

    (set! call-with-output-context
      (lambda (proc parent kop)
        (call-with-keyed-context proc parent kop-key kop)))

    (set! make-top-level-output-port-alist
      (lambda ()
        (let ((kop  (make-kernel-output-port (current-output-port))))
          (suggest-object-name kop "standard-output-port")
          (make-alist
            ()
            kop-key
            kop)))))

  (set! make-top-level-ports-alist
    (lambda ()
      (append (make-top-level-input-port-alist)
              (make-top-level-output-port-alist)))))

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

      'char-ready?
      (action->checked-applicative
        (lambda (operand-tree env context)
          (kernel-char-ready?
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
                                (combine combiner () env context))
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
                                (combine combiner () env context))
                              context
                              kop)))
            (close-kernel-output-port kop context)
            result))
        2 2 string? combiner?)

      )))

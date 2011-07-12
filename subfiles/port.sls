#!r6rs
; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

;;;;;;;;;;;;;;;;
; kernel ports ;
;;;;;;;;;;;;;;;;
;
; A kernel-input-port has type 'input-port, and attribute 'input-port whose
; value is a Scheme input-port.  A kernel-output-port has type 'output-port,
; and attribute 'output-port whose value is a Scheme output-port.
;

(library (subfiles port)
  (export make-top-level-input-port-alist
          make-top-level-ports-alist
          open-kernel-input-file
          close-kernel-input-port
          kernel-input-port?
          kernel-output-port?
          open-kernel-output-file
          close-kernel-output-port
          get-kernel-current-input-port
          kernel-read-char
          kernel-peek-char
          kernel-write
          get-kernel-current-output-port
          kernel-display
          kernel-newline
          kernel-write-char
          call-with-input-context
          call-with-output-context)
  (import (rnrs)
          (subfiles object)
          (subfiles keyed)
          (subfiles revision)
          (subfiles operative)
          (subfiles kernel-pair)
          (subfiles context))

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

; XXX NONEXISTENT
;; (define kernel-char-ready?
;;   (lambda (kip context)
;;     (apply-safely
;;       char-ready?
;;       (list (kip 'input-port))
;;       (list "Failure during char-ready?, " (list kip))
;;       context)))

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

(define kip-key  (get-fresh-key))

(define get-kernel-current-input-port
      (lambda (context)
        (context-keyed-lookup kip-key context)))

(define call-with-input-context
      (lambda (proc parent kip)
        (call-with-keyed-context proc parent kip-key kip)))

(define make-top-level-input-port-alist
      (lambda ()
        (let ((kip  (make-kernel-input-port (current-input-port))))
          (suggest-object-name kip "standard-input-port")
          (make-alist
           '()
           kip-key
           kip))))

(define kop-key  (get-fresh-key))

(define get-kernel-current-output-port
      (lambda (context)
        (context-keyed-lookup kop-key context)))

(define call-with-output-context
      (lambda (proc parent kop)
        (call-with-keyed-context proc parent kop-key kop)))

(define make-top-level-output-port-alist
      (lambda ()
        (let ((kop  (make-kernel-output-port (current-output-port))))
          (suggest-object-name kop "standard-output-port")
          (make-alist
           '()
           kop-key
           kop))))

(define make-top-level-ports-alist
  (lambda ()
    (append (make-top-level-input-port-alist)
            (make-top-level-output-port-alist))))

(set-version (list 0.0 0)
             (list 0.1 0))
(set-revision-date 2007 8 4)

)
